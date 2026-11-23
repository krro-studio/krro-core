(ns top.kzre.krro.core.keymap
  (:require
    [top.kzre.krro.core.util.router :as router]
    [top.kzre.krro.core.command :as cmd]
    [top.kzre.krro.core.hook :as hook]
    [top.kzre.krro.core.message :as msg]
    [top.kzre.krro.core.util :as util]
    [top.kzre.krro.core.variable :refer [*debug*]]))

;; ── 全局键图 ──
(defonce ^:private global-keymap* (atom {}))

(defn global-keymap []
  @global-keymap*)

;; ── 前缀会话 ──
(def ^:private session (router/make-session))

;; ── 构造 ──
(def make-keymap router/build)
(def keymap* router/build)

(defmacro ^:deprecated
  keymap [bindings]
  (keymap* bindings))

(defn set-global-key! [& {:as key-bindings}]
  (let [nested (router/build key-bindings)]
    (swap! global-keymap* util/merge-deep nested)))

;; ── 查询 ──
(def lookup-key router/resolve)
(def lookup-key-in-context router/resolve-chain)

;; ── 处理 ──
(defn handle-key! [key-desc keymaps-list]
  (router/step
    session
    key-desc
    (cons @global-keymap* keymaps-list)
    {:hit    (fn [command]
               (try
                 (cmd/execute-command! command)
                 (catch Exception e
                   (msg/error (str "Command execution failed: " (.getMessage e)))
                   (when *debug* (throw e)))))
     :prefix (fn [node]
               (when-let [prefix-str (:prefix node)]
                 (hook/run-hook! :krro.core/key-sequence-hook prefix-str)))
     :miss   (fn [input]
               (msg/warn (str "Undefined key sequence: " input)))}))

(defn reset-prefix! [] (router/session-reset! session))

(defn describe-key [key-vec keymaps-list]
  (router/describe key-vec (cons @global-keymap* keymaps-list)))