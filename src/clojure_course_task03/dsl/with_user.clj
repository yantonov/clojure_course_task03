(ns clojure-course-task03.dsl.with-user
  (:require [clojure-course-task03.dsl.user :as u]))

(defn value-for-defined-var
  [^clojure.lang.Symbol variable]
  (let [v (ns-resolve (symbol (ns-name *ns*))
                   variable)]
    (if (nil? v) nil @v)))

(defmacro with-user [name & body]
  ;; Sample
  ;; (with-user Ivanov
  ;;   . . .)
  ;; 1) Find all variables starts with Ivanov, в *user-tables-var*s
  ;;    (Ivanov-proposal-fields-var and Ivanov-agents-fields-var)
  ;; 2) Create local variables without prefix Ivanov-
  ;;    proposal-fields-var и agents-fields-var.
  ;;    So select function called inside with-user macro received all necessary variables
  ;; which looks like <table-name>-fields-var.
  (let [user-name (str name)
        strip-prefix-len (inc (.length user-name))]
    (let [bindings (apply vector (apply concat
                                     (for [v (u/get-user-tables-vars)
                                           :when (.startsWith (str v) user-name)]
                                       [(symbol (.substring (str v)
                                                            strip-prefix-len))
                                        (value-for-defined-var v)])))]
      `(let ~bindings ~@body))))
