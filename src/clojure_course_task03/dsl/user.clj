(ns clojure-course-task03.dsl.user
  (:require [clojure-course-task03.dsl.group :as g]))

(defn table-privileges-var-name
  [^clojure.lang.Symbol user-name
   ^clojure.lang.Symbol table-name]
  (symbol (format "%s-%s-fields-var"
                  user-name
                  table-name)))

(defn only-belongs-to-command [security-data-item]
  (let [[cmd] security-data-item]
    (= cmd 'belongs-to)))

(defn get-security-items
  [items]
  (for [item items
        :when (only-belongs-to-command item)]
    (apply vector (map symbol item))))

(defn is-all? [columns]
  (and (= 1 (count columns))
       (=  "::all" (str (first columns)))))

(defmacro user [name & body]
  ;; Sample
  ;; (user Ivanov
  ;;     (belongs-to Agent))
  ;; Creates vars Ivanov-proposal-fields-var = [:person, :phone, :address, :price]
  ;; и Ivanov-agents-fields-var = [:clients_id, :proposal_id, :agent]
  ;; vars
  (let [user-name name
        ns-sym (symbol (ns-name *ns*))
        security-items (get-security-items body)
        defined-var-values (atom {})
        defs
        (apply concat
               (for [group-name (mapcat rest security-items)]
                 (let [group-privileges @(ns-resolve ns-sym
                                                     (g/group-privileges-var-name group-name))]
                   (for [table-name (keys group-privileges)]
                     (let [var-name-to-def (table-privileges-var-name user-name
                                                                      table-name)
                           new-val (map keyword (group-privileges table-name))]
                       (swap! defined-var-values
                              (fn [a]
                                (let [current (a var-name-to-def [])]
                                  (assoc a var-name-to-def
                                         (if (or (is-all? new-val)
                                                 (is-all? current))
                                           [:all]
                                           (vec (concat current new-val)))))))
                       (let [var-value (@defined-var-values var-name-to-def)]
                         `(def ~var-name-to-def ~var-value)))))))]
    `(list ~@defs)))
