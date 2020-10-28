(ns special-lisp.interpreter
  (:refer-clojure :exclude [eval])
  (:require [special-lisp.util :refer [throw+]]))

(def P
  '(do
     (def a 1)

     (def f
       (fn [b]
         (+ a b)))

     (f 3)))

(def global-env
  {'+ (fn [args _]
        (apply + args))})

(defn eval
  [expr & [env]]
  (let [env (or env (atom global-env))]
    (cond
      (symbol? expr)
      (or (@env expr)
          (throw+ expr " is not defined!"))

      ;; constant
      (not (or (list? expr)
               (instance? clojure.lang.Cons expr)))
      expr

      :else
      (let [[op & args] expr]
        (case op
          def
          (let [[name val] args
                val+ (eval val env)]
            (swap! env assoc name val+))

          fn
          (let [[argv body] args
                closed-env @env]
            (fn [actual-args actual-env]
              (let [args-env (zipmap argv actual-args)
                    fn-env (atom (merge closed-env
                                        @actual-env
                                        args-env))]
                (eval body fn-env))))

          do
          (reduce (fn [_ expr]
                    (eval expr env))
                  env
                  args)

          ;; function call
          (let [f (eval op env)]
            (f (map #(eval % env) args)
               env)))))))
