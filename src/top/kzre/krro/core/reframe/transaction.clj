(ns top.kzre.krro.core.reframe.transaction
  "reframe 事务插件（异常安全版：打断时新事务放弃）"
  (:require
    [top.kzre.krro.core.reframe.util :as util]
    [top.kzre.krro.core.message      :as msg]))

(defn valid-instruction? [inst]
  (and (or (vector? inst) (seq? inst) (sequential? inst))
       (contains? #{:begin-transaction :commit-transaction
                    :transaction-operation :rollback-transaction}
                  (first inst))))
;; ─────────────────────────────────────────────────────────────
;; 指令构造
;; ─────────────────────────────────────────────────────────────

(defn ^:deprecated begin-transaction
  "构造事务创建指令"
  [trans-kind & {:as args}]
  [:begin-transaction trans-kind args])

(defn tx-begin
  "构造事务创建指令"
  [trans-kind params]
  [:begin-transaction trans-kind params])

(defn tx-commit
  "构造事务提交指令"
  [trans-kind params]
  [:commit-transaction trans-kind params])

(defn tx-operate
  "构建事务操作指令"
  [trans-kind op-kind params]
  [:transaction-operation trans-kind op-kind params])

(defn tx-rollback
  "构造事务回滚指令"
  [trans-kind params]
  [:rollback-transaction trans-kind params])

(defn ^:deprecated commit-transaction
  "构造事务提交指令"
  [trans-kind & {:as args}]
  [:commit-transaction trans-kind args])

(defn ^:deprecated rollback-transaction
  "构造事务回滚指令"
  [trans-kind & {:as args}]
  [:rollback-transaction trans-kind args])

(defn ^:deprecated transaction-operation
  "构建事务操作指令"
  [trans-kind op-kind & {:as args}]
  [:transaction-operation trans-kind op-kind args])

;; ─────────────────────────────────────────────────────────────
;; 事务协议
;; ─────────────────────────────────────────────────────────────

(defprotocol ITransaction
  (kind    [_])
  (begin   [_ kwargs record])
  (operate [_ op-kind kwargs record])
  (commit  [_ kwargs record])
  (rollback [_ ctx record]))

;; ─────────────────────────────────────────────────────────────
;; 注册表
;; ─────────────────────────────────────────────────────────────

(defonce ^:private transaction-registry (atom {}))

(defn- transactions []
  @transaction-registry)

(defn- transaction [trans-kind]
  (get (transactions) trans-kind))

(defn reg-transaction [kind trans]
  {:pre [(keyword? kind)
         (satisfies? ITransaction trans)]}
  (swap! transaction-registry assoc kind trans))

;; ─────────────────────────────────────────────────────────────
;; 事务状态 key
;; ─────────────────────────────────────────────────────────────

(defonce ^:private transaction-key* ::transaction)
(defn- transaction-key [] transaction-key*)

(defonce ^:private transaction-instructions-key* ::transaction-instructions)
(defn transaction-instructions-key [] transaction-instructions-key*)

;; ─────────────────────────────────────────────────────────────
;; 查询
;; ─────────────────────────────────────────────────────────────

(defn current
  "返回当前活跃的事务"
  [cofx-or-record]
  (or
    (get-in cofx-or-record [:record (transaction-key)])
    (get    cofx-or-record         (transaction-key))))

(defn active?
  "判断当前活跃的事务类型是否是 kind"
  [cofx k]
  (boolean
    (when-let [tx (current cofx)]
      (= (kind tx) k))))

;; ─────────────────────────────────────────────────────────────
;; 安全 rollback：rollback 是纯逻辑，靠返回 result-map 生效，
;; 不是副作用。这里只负责调用并吞掉异常，把 result-map 原样
;; 向上返回，交给 execute-instruction 作为 effects 合并。
;;   · 正常   → 返回 rollback 的 result-map（可能含 :record/:fx/...）
;;   · 异常   → warn 后返回 nil（无效果，等价于空回滚）
;;   · 无事务 → nil
;; 注意：本函数不产生任何副作用，也不负责清空事务状态；
;;       事务状态由调用方返回 nil 事务来结束。
;; ─────────────────────────────────────────────────────────────

(defn- safe-rollback
  [trans ctx record]
  (when trans
    (try
      (rollback trans ctx record)
      (catch Throwable e
        (msg/warn "transaction rollback failed"
                  {:kind  (kind trans)
                   :error e})
        nil))))

;; ─────────────────────────────────────────────────────────────
;; 单条指令执行（永不抛异常）
;; 返回 [new-trans effects]
;;   new-trans 为 nil 表示事务已结束（commit / rollback / 放弃）
;; ─────────────────────────────────────────────────────────────

(defn- execute-instruction
  [trans inst record instruction-acc]
  (try
    (let [[tag & args] inst]
      (case tag

        ;; ── begin ────────────────────────────────────────────
        :begin-transaction
        (let [[k kwargs] args]
          (if-not (keyword? k)
            (do
              (msg/warn "invalid transaction kind, instruction dropped"
                        {:kind k})
              [nil {:fx []}])

            (if trans
              ;; 有活跃事务 → rollback 旧事务，新 begin 放弃
              (do
                (msg/warn "transaction interrupted by begin, rollback old and drop new"
                          {:active     (kind trans)
                           :attempting k})
                [nil (or (safe-rollback trans
                                        {:instructions instruction-acc
                                         :status       :interrupted
                                         :instruction  inst}
                                        record)
                         {:fx []})])

              ;; 无活跃事务 → 正常 begin
              (if-let [t (transaction k)]
                (try
                  (begin t kwargs record)
                  (catch Throwable e
                    (msg/warn "begin transaction failed, instruction dropped"
                              {:kind k :error e})
                    [nil {:fx []}]))
                (do
                  (msg/warn "unknown transaction kind, instruction dropped"
                            {:kind k})
                  [nil {:fx []}])))))

        ;; ── operation ────────────────────────────────────────
        :transaction-operation
        (let [[k op-kind kwargs] args]
          (cond
            (nil? trans)
            (do
              (msg/warn "no active transaction, operation dropped"
                        {:operation op-kind})
              [nil {:fx []}])

            (not= k (kind trans))
            (do
              (msg/warn "transaction kind mismatch, rollback and drop"
                        {:active     (kind trans)
                         :attempting k})
              [nil (or (safe-rollback trans
                                      {:instructions instruction-acc
                                       :status       :error
                                       :instruction  inst}
                                      record)
                       {:fx []})])

            :else
            (try
              (operate trans op-kind kwargs record)
              (catch Throwable e
                (msg/warn "transaction operation failed, rollback"
                          {:kind k :op op-kind :error e})
                [nil (or (safe-rollback trans
                                        {:instructions instruction-acc
                                         :status       :error
                                         :instruction  inst}
                                        record)
                         {:fx []})]))))

        ;; ── commit ───────────────────────────────────────────
        :commit-transaction
        (let [[k kwargs] args]
          (cond
            (nil? trans)
            (do
              (msg/warn "commit without active transaction, instruction dropped")
              [nil {:fx []}])

            (not= k (kind trans))
            (do
              (msg/warn "commit kind mismatch, rollback"
                        {:active     (kind trans)
                         :attempting k})
              [nil (or (safe-rollback trans
                                      {:instructions instruction-acc
                                       :status       :error
                                       :instruction  inst}
                                      record)
                       {:fx []})])

            :else
            (try
              [nil (commit trans kwargs record)]
              (catch Throwable e
                (msg/warn "commit failed, rollback"
                          {:kind k :error e})
                [nil (or (safe-rollback trans
                                        {:instructions instruction-acc
                                         :status       :error
                                         :instruction  inst}
                                        record)
                         {:fx []})]))))

        ;; ── rollback ─────────────────────────────────────────
        :rollback-transaction
        (let [[k] args]
          (if-not trans
            (do
              (msg/warn "rollback without active transaction, noop")
              [nil {:fx []}])
            (do
              (when-not (= k (kind trans))
                (msg/warn "rollback kind mismatch"
                          {:active     (kind trans)
                           :attempting k}))
              [nil (or (safe-rollback trans
                                      {:instructions instruction-acc
                                       :status       :rollback
                                       :instruction  inst}
                                      record)
                       {:fx []})])))

        ;; ── unknown ──────────────────────────────────────────
        (do
          (msg/warn "unknown transaction instruction, dropped"
                    {:instruction inst})
          [trans {:fx []}])))

    (catch Throwable e
      (msg/warn "unexpected error in transaction instruction, rollback"
                {:instruction inst :error e})
      [nil (or
             (and
               trans
               (safe-rollback trans
                              {:instructions instruction-acc
                               :status       :error
                               :instruction  inst}
                              record))

               {:fx []})])))

;; ─────────────────────────────────────────────────────────────
;; 指令序列执行
;; ─────────────────────────────────────────────────────────────

(defn- execute-instructions
  [transaction instructions record ins-acc]
  (loop [trans           transaction
         remaining       instructions
         instruction-acc ins-acc
         current         record
         effects-acc     {:fx []}]
    (if-not (seq remaining)
      [trans (assoc effects-acc :record current)]
      (let [instruction  (first remaining)
            [new-trans effects]
            (execute-instruction trans instruction current instruction-acc)

            effects' (util/merge-effects effects-acc effects)]
        (recur new-trans
               (rest remaining)
               (if (some? new-trans)
                 (conj instruction-acc instruction)
                 [])
               (:record effects')
               (dissoc effects' :record))))))

;; ─────────────────────────────────────────────────────────────
;; re-frame interceptor
;; ─────────────────────────────────────────────────────────────
(defn- compat-inst [v]
  (if (valid-instruction? v) [v] v))

(defn transaction-interceptor
  []
  {:after
   (fn [context]
     (try
       (let [inst-v (compat-inst (get-in context [:effects :transaction]))]
         ;; TODO 移除兼容
         (if (seq inst-v)
           (do
             (when (> (count inst-v) 1)
               (msg/warn "multi instructions is not yet suppurted!!!"))
             (let [record      (get-in context [:effects :record])
                   transaction (get record (transaction-key))
                   inst-acc    (get record (transaction-instructions-key) [])
                   [new-trans trans-effects]
                   (execute-instructions
                     transaction inst-v record inst-acc)

                   trans-effects'
                   (-> trans-effects
                       (assoc-in [:record (transaction-key)]
                                 new-trans)
                       (assoc-in [:record (transaction-instructions-key)]
                                 (if (some? new-trans)
                                   (get record (transaction-instructions-key))
                                   [])))]
               (update context :effects util/merge-effects trans-effects')))
           context))
       (catch Throwable e
         (msg/warn "transaction interceptor fatal error" {:error e})
         context)))})
