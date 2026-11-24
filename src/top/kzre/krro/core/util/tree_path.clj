(ns top.kzre.krro.core.util.tree-path
  "树林/树的路径化描述。
   基础模块，被其他模块依赖，但不依赖别人。

   路径是索引向量，如 [2 0] 表示根列表第 2 个节点的第 0 个子节点。
   空向量 [] 表示根容器本身。

   ═══════════════════════════════════════════════
   设计原则
   ═══════════════════════════════════════════════

   本命名空间把\"树操作\"分解为两段：

     第一段：节点查找 → path
       通过谓词或 id 在树中搜索，得到路径

     第二段：path → path（导航）+ path → node（访问）
       路径之间可以导航（上/下兄弟、父子、遍历）
       路径可以访问节点、兄弟列表、子路径

   外部操作只针对路径：
     - 不直接操作树结构
     - 不关心节点语义
     - 通过 path 完成所有定位

   这样树操作的复杂度从\"递归树操作\"降为\"向量操作 + 树查询\"——
   路径是稳定的中间抽象。

   ═══════════════════════════════════════════════
   使用模式
   ═══════════════════════════════════════════════

     ;; 定义子节点访问函数
     (def children-fn :layers)

     ;; 查找 → path
     (def p (get-path-by-id layers :layer-42 children-fn))

     ;; path → node
     (get-node layers p children-fn)

     ;; path → path（导航）
     (above-sibling-path layers p children-fn)
     (parent p)

     ;; path → 其他节点
     (get-node layers (above-sibling-path layers p children-fn) children-fn)

   参数顺序遵循 Clojure 惯例：layers 优先，children-fn 最后。
   检索函数统一使用 get- 前缀。

   ═══════════════════════════════════════════════
   职责边界
   ═══════════════════════════════════════════════

   本命名空间：
     ✓ 路径构造、变换、比较
     ✓ 按谓词/id 查路径、按路径查节点
     ✓ 导航（父、子、兄弟、遍历）

   不属于本命名空间：
     ✗ 树数据更新——由外部组合 update / assoc
     ✗ 节点属性的读取/修改——属于业务命名空间
     ✗ 节点的语义判断（可见性、类型）——属于业务定义

   ═══════════════════════════════════════════════
   children-fn
   ═══════════════════════════════════════════════

   children-fn 是 (fn [node] -> vector of children) 或 key。
   仅用于非根层级——根级直接是树林向量，不需要它。
   放在参数列表最后，因为它是可选的最不重要的参数。")

;; ═══════════════════════════════════════════════
;; 路径构造（纯路径操作，无需树）
;; ═══════════════════════════════════════════════

(defn root
  "从索引构造根级路径。"
  [index]
  [index])

(defn child
  "在父路径后追加索引，形成子路径。"
  [path index]
  (conj path index))

(defn append
  "在路径后追加一个索引。等于 child。"
  [path idx]
  (conj path idx))

(defn parent
  "获取路径的父路径。根级路径返回空向量。"
  [path]
  (if (<= (count path) 1)
    []
    (vec (butlast path))))

(defn with-index
  "替换路径的最后一段索引。"
  [path new-idx]
  (conj (parent path) new-idx))

(defn truncate
  "截断到指定深度。"
  [path n]
  (subvec path 0 (min n (count path))))

(defn path?
  "判断是否为合法路径（非负整数向量，空向量表示根）。"
  [x]
  (and (vector? x)
       (every? #(and (integer? %) (not (neg? %))) x)))

(defn root?
  "路径是否是根级路径（深度 ≤ 1）。注意：[0] 也是根级路径。"
  [path]
  (empty? (parent path)))

(defn depth
  "路径深度。根级 = 1，空路径 = 0。"
  [path]
  (count path))

(defn index-in-parent
  "当前路径在父容器中的索引。空路径返回 nil。"
  [path]
  (when (seq path) (last path)))

;; ═══════════════════════════════════════════════
;; 层级关系（纯路径操作）
;; ═══════════════════════════════════════════════

(defn ancestor?
  "a 是否是 b 的祖先路径。[1] 是 [1 2] [1 2 3] 的祖先。相等不算祖先。"
  [a b]
  (and (< (count a) (count b))
       (= a (subvec b 0 (count a)))))

(defn descendant?
  "a 是否是 b 的后代路径。"
  [a b]
  (ancestor? b a))

(defn sibling?
  "同一父下的不同路径。"
  [a b]
  (and (not= a b)
       (= (parent a) (parent b))))

(defn within?
  "a 是否在 b 的子树内（含 b 自身）。"
  [a b]
  (or (= a b) (ancestor? b a)))

(defn common-ancestor
  "两个路径的最深公共祖先路径。"
  [a b]
  (let [n (min (count a) (count b))]
    (loop [i 0]
      (if (and (< i n) (= (nth a i) (nth b i)))
        (recur (inc i))
        (subvec a 0 i)))))

(defn path<
  "按 DFS 前序遍历，a 是否严格在 b 之前。"
  [a b]
  (cond
    (= a b)         false
    (ancestor? a b) true
    (ancestor? b a) false
    :else
    (let [ca (common-ancestor a b)
          ia (nth a (count ca))
          ib (nth b (count ca))]
      (< ia ib))))

;; ═══════════════════════════════════════════════
;; 树访问（layers = 树林向量，children-fn 最后）
;; ═══════════════════════════════════════════════

(defn get-node
  "按索引路径查找节点。路径无效返回 nil。
   children-fn: (fn [node] -> vector)，仅用于非根层级取子节点。"
  [layers path children-fn]
  (reduce (fn [node idx]
            (if (vector? node)
              (nth node idx nil)
              (let [ch (children-fn node)]
                (nth ch idx nil))))
          layers
          path))

(defn children
  "获取路径指向位置的子节点向量。空路径返回 layers 本身。"
  [layers path children-fn]
  (if (empty? path)
    layers
    (when-let [node (get-node layers path children-fn)]
      (if (vector? node)
        node
        (children-fn node)))))

(defn child-paths
  "父路径的所有子路径——升序。"
  [layers parent-path children-fn]
  (when-let [ch (children layers parent-path children-fn)]
    (mapv #(conj parent-path %) (range (count ch)))))

(defn first-child-path
  "父路径的第一个子路径。无子返回 nil。"
  [layers parent-path children-fn]
  (when (pos? (count (children layers parent-path children-fn)))
    (conj parent-path 0)))

(defn last-child-path
  "父路径的最后一个子路径。无子返回 nil。"
  [layers parent-path children-fn]
  (when-let [ch (children layers parent-path children-fn)]
    (when (seq ch)
      (conj parent-path (dec (count ch))))))

(defn above-last-child-path
  "父路径的最后一个子路径之后的虚拟路径。"
  [layers parent-path children-fn]
  (when-let [ch (children layers parent-path children-fn)]
    (conj parent-path (count ch))))

(defn top-path
  "根容器顶层（末尾）路径。空树林返回 nil。"
  [layers]
  (when (seq layers)
    [(dec (count layers))]))

(defn above-top-path
  "最顶层之上的虚拟路径。"
  [layers]
  [(count layers)])

(defn bottom-path
  "根容器底层（索引 0）路径。空树林返回 nil。"
  [layers]
  (when (seq layers)
    [0]))

(defn above-sibling-path
  "同一父容器中当前路径的下一个兄弟路径。已是最后一个返回 nil。"
  [layers path children-fn]
  (let [parent-path (parent path)
        siblings (children layers parent-path children-fn)]
    (when siblings
      (let [idx (last path)
            next-idx (inc idx)]
        (when (< next-idx (count siblings))
          (conj parent-path next-idx))))))

(defn unchecked-above-sibling-path
  "返回在 layer-id 上方插入新图层时应使用的路径。"
  [path]
  (let [parent (butlast path)
        idx (last path)]
    (conj (vec parent) (inc idx))))

(defn below-sibling-path
  "同一父容器中当前路径的前一个兄弟路径。已是第一个返回 nil。"
  [layers path _children-fn]
  (let [idx (last path)
        prev-idx (dec idx)]
    (when (not (neg? prev-idx))
      (conj (parent path) prev-idx))))

(defn above-sibling-paths
  "同一父容器中，当前路径上方（索引更大）的所有兄弟路径。索引升序。"
  [layers path children-fn]
  (if (empty? path)
    []
    (let [siblings (children layers (parent path) children-fn)
          n (count siblings)
          idx (last path)
          parent-p (parent path)]
      (mapv #(conj parent-p %) (range (inc idx) n)))))



(defn below-sibling-paths
  "同一父容器中，当前路径下方（索引更小）的所有兄弟路径。索引升序。"
  [layers path children-fn]
  (if (empty? path)
    []
    (let [siblings (children layers (parent path) children-fn)
          idx (last path)
          parent-p (parent path)]
      (mapv #(conj parent-p %) (range idx)))))

(defn sibling-range-paths
  "同一父下，从 from-idx 到 to-idx（不含）的所有路径。"
  [parent-path from-idx to-idx]
  (mapv #(conj parent-path %) (range from-idx to-idx)))

(defn paths-between
  "同一父下，从 a 到 b（不含 b）的所有路径。a、b 必须同父。"
  [a b]
  (let [pa (parent a)
        pb (parent b)]
    (when (= pa pb)
      (sibling-range-paths pa (last a) (last b)))))

(defn next-path
  "按 DFS 前序遍历，下一个路径。优先进入子节点，无子找兄弟，无兄弟回溯。"
  [layers path children-fn]
  (or (first-child-path layers path children-fn)
      (loop [p path]
        (when (seq p)
          (or (above-sibling-path layers p children-fn)
              (recur (parent p)))))))

(defn prev-path
  "按 DFS 前序遍历，前一个路径。取前一个兄弟的最深最后一个后代。"
  [layers path children-fn]
  (when-let [prev-sibling (below-sibling-path layers path children-fn)]
    (loop [p prev-sibling]
      (if-let [last-child (last-child-path layers p children-fn)]
        (recur last-child)
        p))))

(defn get-path-by-pred
  "在树林中查找满足条件的节点的索引路径。未找到返回 nil。"
  [layers pred children-fn]
  (letfn [(walk [node path]
            (if (pred node)
              path
              (if-let [ch (if (vector? node) node (children-fn node))]
                (some (fn [[idx child]]
                        (walk child (conj path idx)))
                      (map-indexed vector ch))
                nil)))]
    (some (fn [[idx node]]
            (walk node [idx]))
          (map-indexed vector layers))))

(defn get-path-by-id
  "通过 :id 查找路径。节点需包含 :id 字段。"
  [layers id children-fn]
  (get-path-by-pred layers #(= (:id %) id) children-fn))

;; ═══════════════════════════════════════════════
;; 排序
;; ═══════════════════════════════════════════════

(defn sort-paths
  "对 paths 按 DFS 前序排序。返回排序后的 paths。"
  [paths]
  (vec (sort path< paths)))

(defn sort-by-path
  "paths 和 attachments 是一一对应的扁平序列。
   按 paths 的 DFS 前序排序，attachments 跟随移动。
   返回 [sorted-paths sorted-attachments]。"
  [paths attachments]
  (let [pairs (map vector paths attachments)
        sorted (sort-by first path< pairs)]
    [(mapv first sorted) (mapv second sorted)]))


;; ═══════════════════════════════════════════════
;; 树修改（通用，children-fn 最后）
;; ═══════════════════════════════════════════════

(defn insert-node
  "在树林 layers 的指定路径处插入 node。返回新的树林向量。
   children-fn: (fn [node] -> vector of children)
   assoc-children-fn: (fn [node children] -> node) — 将子列表写回节点
   空路径表示插入到末尾。"
  [layers path node
   children-fn assoc-children-fn]
  (if (seq path)
    (let [idx (last path)
          parent-path (butlast path)]
      (if (seq parent-path)
        ;; 嵌套：递归到父节点内部
        (let [parent-node (get-node layers parent-path children-fn)
              parent-children (children-fn parent-node)
              new-parent-children (insert-node parent-children (subvec path 1) node children-fn assoc-children-fn)
              new-parent (assoc-children-fn parent-node new-parent-children)]
          (assoc-in layers parent-path new-parent))
        ;; 根级插入
        (vec (concat (subvec layers 0 idx) [node] (subvec layers idx)))))
    ;; 空路径：插入到末尾
    (conj (vec layers) node)))

(defn remove-node
  "从树林 layers 的指定路径移除节点。返回 [new-layers removed-node]。"
  [layers path
   children-fn assoc-children-fn]
  (when (seq path)
    (let [idx (last path)
          parent-path (butlast path)]
      (if (seq parent-path)
        ;; 嵌套：递归删除
        (let [parent-node (get-node layers parent-path children-fn)
              parent-children (children-fn parent-node)
              [new-children removed] (remove-node parent-children (subvec path 1) children-fn assoc-children-fn)
              new-parent (assoc-children-fn parent-node new-children)]
          [(assoc-in layers parent-path new-parent) removed])
        ;; 根级删除
        (let [removed (nth layers idx)
              new-layers (vec (concat (subvec layers 0 idx) (subvec layers (inc idx))))]
          [new-layers removed])))))

(defn move-node
  "将节点从 old-path 移动到 new-path，返回新的树林。
   old-path 和 new-path 均为索引向量。"
  [layers old-path new-path
   children-fn assoc-children-fn]
  (when (and (seq old-path) (seq new-path))
    (let [[temp-layers removed] (remove-node layers old-path children-fn assoc-children-fn)
          same-parent? (= (butlast old-path) (butlast new-path))
          old-idx (last old-path)
          new-idx (last new-path)
          adjusted-new-path (if (and same-parent? (< old-idx new-idx))
                              (conj (butlast new-path) (dec new-idx))
                              new-path)]
      (insert-node temp-layers adjusted-new-path removed children-fn assoc-children-fn))))

(defn update-node
  "对指定路径处的节点应用更新函数 f，返回新的树林。
   f 接收当前节点，返回更新后的节点。路径无效时返回原 layers。"
  [layers path f
   children-fn assoc-children-fn]
  (when (seq path)
    (let [idx (last path)
          parent-path (butlast path)]
      (if (seq parent-path)
        ;; 嵌套：递归更新
        (let [parent-node (get-node layers parent-path children-fn)
              parent-children (children-fn parent-node)
              new-parent-children (update-node parent-children (subvec path 1) f children-fn assoc-children-fn)
              new-parent (assoc-children-fn parent-node new-parent-children)]
          (assoc-in layers parent-path new-parent))
        ;; 根级更新
        (assoc layers idx (f (nth layers idx)))))))