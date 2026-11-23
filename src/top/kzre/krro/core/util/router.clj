(ns top.kzre.krro.core.util.router
  "通用分层路由器。支持嵌套路由表、分步匹配（前缀会话）、优先级链。
   不绑定任何业务语义（快捷键、i18n、theme 都可用）。"
  (:refer-clojure :exclude [resolve]))

;; ── 路由表构造 ──────────────────────────────

(defn build
  "根据平面绑定表生成嵌套路由表。
   bindings 格式：{path value}
   path 可以是：
     - 单个 key（如 :u）
     - key 向量（如 [:C-x :u]）
   返回嵌套 map。
   示例：
     (build {:u :undo, [:C-x :u] :save})
     => {:u :undo, :C-x {:u :save}}"
  [bindings]
  (reduce-kv
    (fn [m path value]
      (let [path-vec (if (vector? path) path [path])]
        (if (seq path-vec)
          (try
            (assoc-in m path-vec value)
            (catch ClassCastException e
              (throw (ex-info (str "Cannot bind path " path-vec
                                   " because prefix is already a leaf")
                              {:path path-vec
                               :existing (get-in m (butlast path-vec))}
                              e))))
          (throw (ex-info "Path must not be empty" {})))))
    {}
    bindings))

(defmacro defrouter
  "定义并构建路由表。
   示例：
     (defrouter my-router {:u :undo, [:C-x :u] :save})"
  [name bindings]
  `(def ~name (build ~bindings)))


;; ── 匹配 ────────────────────────────────────

(defn resolve
  "在路由表中查找 path-vec。
   path-vec 为 key 的 vector，如 [:C-x :u]。
   返回：
     - 叶子值（任意类型）
     - map：前缀节点（需要继续匹配）
     - nil：未命中"
  [route-table path-vec]
  (when (and route-table (seq path-vec))
    (let [first-key (first path-vec)]
      (if-let [node (get route-table first-key)]
        (if (map? node)
          (if-let [rest-keys (seq (rest path-vec))]
            (resolve node (vec rest-keys))
            node)
          node)
        nil))))

(defn resolve-chain
  "在路由表列表中按优先级查找。头到尾递减。
   返回第一个非 nil 结果。"
  [path-vec route-tables]
  (some #(resolve % path-vec) route-tables))


;; ── 会话（分步匹配） ────────────────────────

(defn make-session
  "创建一个路由会话，用于分步匹配（如多键序列）。
   返回 session atom，结构：
     {:stack  []     ; 已输入的前缀节点列表
      :table  nil    ; 当前活跃的子路由表
      }"
  []
  (atom {:stack []
         :table nil}))

(defn session-active? [session]
  (boolean (seq (:stack @session))))

(defn session-reset! [session]
  (reset! session {:stack [] :table nil}))

(defn session-push! [session node]
  (swap! session update :stack conj node))

(defn session-pop! [session]
  (swap! session update :stack pop))


;; ── 分步匹配引擎 ────────────────────────────

(defn step
  "在会话上下文中处理单步输入。
   参数：
     session    - make-session 创建的会话
     input      - 当前输入的 key
     route-tables - 路由表列表（优先级从高到低）
     handlers   - {:hit    fn[value] -> 处理命中
                   :prefix fn[node] -> 处理前缀命中
                   :miss   fn[input] -> 处理未命中}
   返回 handlers 的返回值。"
  [session input route-tables handlers]
  (let [{:keys [hit prefix miss]} handlers
        effective-tables (if-let [prefix-node (peek (:stack @session))]
                           (cons prefix-node route-tables)
                           route-tables)
        path-vec [input]
        result (resolve-chain path-vec effective-tables)]
    (cond
      (map? result)
      (do
        (session-push! session result)
        (prefix result))

      (some? result)
      (do
        (session-reset! session)
        (hit result))

      :else
      (do
        (session-reset! session)
        (miss input)))))


;; ── 合并 ────────────────────────────────────

(defn merge-routers
  "深度合并多个路由表，后面的覆盖前面的同名叶子。"
  [base & others]
  (if (empty? others)
    base
    (apply merge-with
           (fn [a b]
             (if (and (map? a) (map? b))
               (merge-routers a b)
               b))
           base
           others)))


;; ── 描述/调试 ──────────────────────────────

(defn describe
  "描述 path-vec 在路由表列表中的命中情况。"
  [path-vec route-tables]
  (keep #(resolve % path-vec) route-tables))

(defn flatten-routes
  "将嵌套路由表展平为 {path-vec value} 形式，方便调试。"
  ([route-table] (flatten-routes route-table []))
  ([route-table prefix]
   (reduce-kv
     (fn [acc k v]
       (let [path (conj prefix k)]
         (if (map? v)
           (merge acc (flatten-routes v path))
           (assoc acc path v))))
     {}
     route-table)))