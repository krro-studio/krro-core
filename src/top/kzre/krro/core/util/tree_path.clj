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

     ;; 定义子节点读写函数
     (def children-fn       :layers)                  ;;
     (def children-fn       #(:layers %))             ;; 读
     (def assoc-children-fn (fn [n cs] (assoc n :layers cs)))  ;; 写

     ;; 查找 → path
     (def p (get-path-by-id nodes :layer-42 :id children-fn))
     (def p (get-path-by-pred nodes #(= (:id %) :layer-42) children-fn))

     ;; path → node
     (get-node nodes p children-fn)

     ;; path → path（导航）
     (above-sibling-path nodes p children-fn)
     (parent p)

     ;; 数据更新
     (insert-node nodes [1 0] new-node children-fn assoc-children-fn)
     (remove-node nodes p children-fn assoc-children-fn)
     (move-node nodes p q children-fn assoc-children-fn)
     (update-node nodes p f children-fn assoc-children-fn)

   参数顺序遵循 Clojure 惯例：nodes 优先，children-fn / assoc-children-fn 最后。
   检索函数统一使用 get- 前缀。

   ═══════════════════════════════════════════════
   职责边界
   ═══════════════════════════════════════════════

   本命名空间：
     ✓ 路径构造、变换、比较
     ✓ 按谓词/id 查路径、按路径查节点
     ✓ 导航（父、子、兄弟、遍历）
     ✓ 通用的树节点插入 / 删除 / 移动 / 更新

   不属于本命名空间：
     ✗ 节点属性的读取/修改——属于业务命名空间
     ✗ 节点的语义判断（可见性、类型）——属于业务定义

   ═══════════════════════════════════════════════
   children-fn / assoc-children-fn
   ═══════════════════════════════════════════════

   children-fn:       (fn [node] -> children-vector)
   assoc-children-fn: (fn [node new-children] -> node)

   两者都是普通函数，直接调用，不做任何类型判断。
   仅作用于非根层级——根级直接是树林向量，不需要它们。

   对 nil 节点，children-of 返回 nil，所有下游函数据此安全短路。")

;; ═══════════════════════════════════════════════
;; 内部辅助：子节点读写
;; ═══════════════════════════════════════════════

(defn- children-of
  "读取节点的子节点向量。node 为 nil 时返回 nil。"
  [node children-fn]
  (when node
    (children-fn node)))

(defn- assoc-children-of
  "将节点的子节点替换为 new-children，返回新节点。"
  [node assoc-children-fn new-children]
  (assoc-children-fn node new-children))

;; ═══════════════════════════════════════════════
;; 路径构造（纯路径操作）
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
  "路径是否是根级路径（深度 ≤ 1）。
   注意：[0] 也是根级路径。"
  [path]
  (empty? (parent path)))

;; ═══════════════════════════════════════════════
;; 层级关系（纯路径操作）
;; ═══════════════════════════════════════════════

(defn ancestor?
  "a 是否是 b 的祖先路径。
   [1] 是 [1 2] [1 2 3] 的祖先。相等不算祖先。"
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
  "child 是否在 parent 的子树内（含 parent 自身）。"
  [child parent]
  (or (= child parent) (ancestor? parent child)))

(defn common-ancestor
  "两个路径的最深公共祖先路径。"
  [a b]
  (let [n (min (count a) (count b))]
    (loop [i 0]
      (if (and (< i n) (= (nth a i) (nth b i)))
        (recur (inc i))
        (subvec a 0 i)))))

(defn depth
  "路径深度。根级 = 1，空路径 = 0。"
  [path]
  (count path))

(defn index-in-parent
  "当前路径在父容器中的索引。空路径返回 nil。"
  [path]
  (when (seq path) (last path)))

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

(defn sort-paths
  "对 paths 按 DFS 前序排序。返回排序后的 paths。"
  [paths]
  (vec (sort path< paths)))

(defn sort-by-path
  "paths 和 attachments 是一一对应的扁平序列。
   按 paths 的 DFS 前序排序，attachments 跟随移动。
   返回 [sorted-paths sorted-attachments]。"
  [paths attachments]
  (let [pairs   (map vector paths attachments)
        sorted  (sort-by first path< pairs)]
    [(mapv first sorted) (mapv second sorted)]))

;; ═══════════════════════════════════════════════
;; 第一段：节点查找 → path
;; ═══════════════════════════════════════════════

(defn get-path-by-pred
  "在树林 nodes 中查找第一个满足 pred 的节点，返回其路径。
   未找到返回 nil。"
  [nodes pred children-fn]
  (some (fn [[idx node]]
          (if (pred node)
            [idx]
            (when-let [cs (children-of node children-fn)]
              (when (seq cs)
                (when-let [sub (get-path-by-pred cs pred children-fn)]
                  (into [idx] sub))))))
        (map-indexed vector nodes)))

(defn get-path-by-id
  "在树林 nodes 中按 id 查找路径。id-fn 从节点提取 id。
   未找到返回 nil。"
  [nodes id id-fn children-fn]
  (get-path-by-pred nodes #(= id (id-fn %)) children-fn))

;; ═══════════════════════════════════════════════
;; 第二段：path → node
;; ═══════════════════════════════════════════════

(defn get-node
  "按索引路径查找节点。空路径返回 nodes 本身。
   路径无效返回 nil。"
  [nodes path children-fn]
  (if (empty? path)
    nodes
    (reduce (fn [node idx]
              (let [cs (children-of node children-fn)]
                (nth cs idx nil)))
            (nth nodes (first path) nil)
            (rest path))))

(defn get-children
  "按路径获取节点的子节点向量。
   空路径返回 nodes 本身；路径无效或无子返回 nil。"
  [nodes path children-fn]
  (if (empty? path)
    nodes
    (children-of (get-node nodes path children-fn) children-fn)))

(defn get-siblings
  "路径所指向的兄弟节点，包括自己。空路径返回 nodes 本身。"
  [nodes path children-fn]
  (if (empty? path)
    nodes
    (get-children nodes (parent path) children-fn)))

(defn child-paths
  "父路径的所有子路径——升序。"
  [nodes parent-path children-fn]
  (let [cs (get-children nodes parent-path children-fn)]
    (mapv #(conj parent-path %) (range (count cs)))))

(defn first-child-path
  "父路径的第一个子路径。无子返回 nil。"
  [nodes parent-path children-fn]
  (let [cs (get-children nodes parent-path children-fn)]
    (when (seq cs)
      (conj parent-path 0))))

(defn last-child-path
  "父路径的最后一个子路径。无子返回 nil。"
  [nodes parent-path children-fn]
  (let [cs (get-children nodes parent-path children-fn)]
    (when (seq cs)
      (conj parent-path (dec (count cs))))))

(defn above-last-child-path
  "父路径的最后一个子路径之后的虚拟路径。"
  [nodes parent-path children-fn]
  (let [cs (get-children nodes parent-path children-fn)]
    (conj parent-path (count cs))))

;; ═══════════════════════════════════════════════
;; 第二段：path → path（导航）
;; ═══════════════════════════════════════════════

(defn top-path
  "顶层（末尾）路径。列表为空返回 nil。"
  [nodes]
  (when (seq nodes)
    [(dec (count nodes))]))

(defn above-top-path
  "最顶层节点之上的虚拟路径。"
  [nodes]
  [(count nodes)])

(defn bottom-path
  "底层（索引 0）路径。列表为空返回 nil。"
  [nodes]
  (when (seq nodes)
    [0]))

(defn above-sibling-path
  "同一父容器中当前路径的下一个兄弟路径。已是最后一个返回 nil。"
  [nodes path children-fn]
  (let [siblings (get-siblings nodes path children-fn)
        idx      (last path)
        next-idx (inc idx)]
    (when (< next-idx (count siblings))
      (conj (parent path) next-idx))))

(defn unchecked-above-sibling-path
  "返回在当前路径上方插入新节点时应使用的路径。"
  [path]
  (let [pp  (butlast path)
        idx (last path)]
    (conj (vec pp) (inc idx))))

(defn above-sibling-paths
  "同一父容器中，当前路径上方（索引更大）的所有兄弟路径。
   按索引升序返回。空路径或当前已是最后一个时返回 []。"
  [nodes path children-fn]
  (if (empty? path)
    []
    (let [siblings (get-siblings nodes path children-fn)
          n        (count siblings)
          idx      (last path)
          pp       (parent path)]
      (mapv #(conj pp %) (range (inc idx) n)))))

(defn below-sibling-path
  "同一父容器中当前路径的前一个兄弟路径。已是第一个返回 nil。"
  [_nodes path]
  (let [idx      (last path)
        prev-idx (dec idx)]
    (when (not (neg? prev-idx))
      (conj (parent path) prev-idx))))

(defn below-sibling-paths
  "同一父容器中，当前路径下方（索引更小）的所有兄弟路径。
   按索引升序返回。空路径或当前已是第一个时返回 []。"
  [nodes path children-fn]
  (if (empty? path)
    []
    (let [siblings (get-siblings nodes path children-fn)
          n        (count siblings)
          idx      (min (last path) n)
          pp       (parent path)]
      (mapv #(conj pp %) (range idx)))))

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
  "按 DFS 前序遍历，下一个路径。
   优先进入子节点——若无子则找下一个兄弟——若无兄弟则回溯到祖先的兄弟。
   遍历结束返回 nil。"
  [nodes path children-fn]
  (or (first-child-path nodes path children-fn)
      (loop [p path]
        (when (seq p)
          (or (above-sibling-path nodes p children-fn)
              (recur (parent p)))))))

(defn prev-path
  "按 DFS 前序遍历，前一个路径。
   若有兄弟——取兄弟的最深最后一个后代。"
  [nodes path children-fn]
  (when-let [prev-sibling (below-sibling-path nodes path)]
    (loop [p prev-sibling]
      (if-let [last-child (last-child-path nodes p children-fn)]
        (recur last-child)
        p))))

;; ═══════════════════════════════════════════════
;; 数据更新（树节点操作）
;; ═══════════════════════════════════════════════

(defn insert-node
  "在树林 nodes 的指定路径处插入 node。返回新的节点向量。
   path 为空表示插入到末尾。"
  [nodes path node children-fn assoc-children-fn]
  (if (empty? path)
    (conj (vec nodes) node)
    (let [idx         (last path)
          parent-path (butlast path)]
      (if (empty? parent-path)
        (vec (concat (subvec nodes 0 idx)
                     [node]
                     (subvec nodes idx)))
        (let [parent-idx   (first parent-path)
              parent       (nth nodes parent-idx)
              children     (children-of parent children-fn)
              new-children (insert-node children (rest path) node
                                        children-fn assoc-children-fn)
              new-parent   (assoc-children-of parent assoc-children-fn new-children)]
          (assoc nodes parent-idx new-parent))))))

(defn remove-node
  "从树林 nodes 的指定路径移除节点。返回 [new-nodes removed-node]。
   path 为空时返回 nil。"
  [nodes path children-fn assoc-children-fn]
  (when (seq path)
    (let [idx         (last path)
          parent-path (butlast path)]
      (if (empty? parent-path)
        (let [removed  (nth nodes idx)
              new-nodes (vec (concat (subvec nodes 0 idx)
                                     (subvec nodes (inc idx))))]
          [new-nodes removed])
        (let [parent-idx   (first parent-path)
              parent       (nth nodes parent-idx)
              children     (children-of parent children-fn)
              [new-children removed] (remove-node children (rest path)
                                                  children-fn assoc-children-fn)
              new-parent   (assoc-children-of parent assoc-children-fn new-children)]
          [(assoc nodes parent-idx new-parent) removed])))))

(defn move-node
  "将节点从 old-path 移动到 new-path（new-path 语义为\"插入到该位置\"）。
   同一父下移动时自动修正因先删除造成的索引偏移。
   返回新的树林。"
  [nodes old-path new-path children-fn assoc-children-fn]
  (when (and (seq old-path) (seq new-path))
    (let [[temp-nodes removed] (remove-node nodes old-path
                                            children-fn assoc-children-fn)
          same-parent? (= (butlast old-path) (butlast new-path))
          old-idx      (last old-path)
          new-idx      (last new-path)
          adjusted     (if (and same-parent? (< old-idx new-idx))
                         (conj (butlast new-path) (dec new-idx))
                         new-path)]
      (insert-node temp-nodes adjusted removed
                   children-fn assoc-children-fn))))

(defn update-node
  "对指定路径处的节点应用更新函数 f，返回新的树林。
   f 接收当前节点，返回更新后的节点。路径为空时返回 nil。"
  [nodes path f children-fn assoc-children-fn]
  (when (seq path)
    (let [idx         (last path)
          parent-path (butlast path)]
      (if (empty? parent-path)
        (assoc nodes idx (f (nth nodes idx)))
        (let [parent-idx   (first parent-path)
              parent       (nth nodes parent-idx)
              children     (children-of parent children-fn)
              new-children (update-node children (rest path) f
                                        children-fn assoc-children-fn)
              new-parent   (assoc-children-of parent assoc-children-fn new-children)]
          (assoc nodes parent-idx new-parent))))))

(defn run-path!
  "沿 path 逐层向下遍历树林。

  nodes         - 根级节点向量（树林）
  path          - 索引路径，如 [2 0 1]
  f             - (fn [path node ])，node 是当前层命中的节点，path 是从根到该节点的路径
  children-fn   - (fn [node] -> 子节点向量)

  从根层开始，按 path 每一段索引逐层深入，每到达一个节点就调用 f。
  遍历顺序：根 → 中间节点 → 叶子。无返回值。"
  [nodes path f children-fn]
  (when (seq path)
    (loop [forest     nodes
           idxs       path
           path-so-far []]
      (when (seq idxs)
        (let [i        (first idxs)
              node     (nth forest i nil)]
          (when node
            (let [reached (conj path-so-far i)]
              (f reached node)
              (when-let [children (children-of node children-fn)]
                (recur children (rest idxs) reached)))))))))

(defn prune-subpaths
  "路径规约：若 paths 中同时存在祖先和后代路径，去掉后代，只保留祖先。
  顺便去重"
  [paths]
  (if (empty? paths)
    []
    (let [sorted (sort-paths paths)]
      (reduce (fn [acc path]
                (if (some #(within? % path) acc)
                  acc
                  (conj acc path)))
              []
              sorted))))