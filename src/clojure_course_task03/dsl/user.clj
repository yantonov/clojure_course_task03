(ns clojure-course-task03.dsl.user
  (:require [clojure-course-task03.dsl.group :as g]))

(def ^:dynamic *user-tables-vars* (atom #{}))

(defn add-var-to-user-tables-vars [var-name]
  (swap! *user-tables-vars* conj var-name))

(defn remove-var-to-user-tables-vars [var-name]
  (swap! *user-tables-vars*
         (fn [reg] (disj reg var-name))))

(defn get-user-tables-vars []
  @*user-tables-vars*)

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
       (= :all (first columns))))

(defmacro user [name & body]
  ;; Sample
  ;; (user Ivanov
  ;;     (belongs-to Agent))
  ;; Creates vars Ivanov-proposal-fields-var = [:person, :phone, :address, :price]
  ;; и Ivanov-agents-fields-var = [:clients_id, :proposal_id, :agent]
  ;; vars
  ;;
  ;; Saves this variables to *user-tables-vars* atom.
  (let [user-name name
        security-items (get-security-items body)
        defined-var-values (atom {})
        ns-sym (symbol (ns-name *ns*))]
    (doseq [group-name (mapcat rest security-items)]
      (doseq [table-name (keys (g/group-privileges group-name))]
        (let [var-name-to-def (table-privileges-var-name user-name
                                                         table-name)
              new-val (g/table-privileges-as-keywords
                       (g/table-privileges group-name
                                           table-name))]
          (swap! defined-var-values
                 (fn [a]
                   (let [current (a var-name-to-def [])]
                     (assoc a var-name-to-def
                            (if (or (is-all? new-val)
                                    (is-all? current))
                              [:all]
                              (vec (concat current new-val)))))))
          (intern ns-sym
                  var-name-to-def
                  (@defined-var-values var-name-to-def))
          (add-var-to-user-tables-vars var-name-to-def))))))
