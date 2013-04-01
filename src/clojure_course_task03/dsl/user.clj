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

(defn var-name-which-holds-table-privileges
  [^clojure.lang.Symbol user-name
   ^clojure.lang.Symbol table-name]
  (symbol (str user-name
               "-"
               table-name
               "-"
               "fields-var")))

(defn only-belongs-to-command [security-data-item]
  (let [[cmd] security-data-item]
    (= cmd 'belongs-to)))

(defn get-security-items
  [items]
  (for [item items
        :when (only-belongs-to-command item)]
    (let [[cmd group] item]
      (vector (symbol cmd)
              (symbol group)))))

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
        security-items (get-security-items body)]
    (doseq [security-data-item security-items]
      (let [[_ group-name] security-data-item
            privileges (g/group-privileges group-name)
            table-names (keys privileges)]
        (doseq [table-name table-names]
          (let [var-name-to-def
                (var-name-which-holds-table-privileges user-name table-name)]
            (intern (symbol (ns-name *ns*))
                    var-name-to-def
                    (g/table-privileges-as-keywords (g/table-privileges group-name table-name)))
            (add-var-to-user-tables-vars var-name-to-def)))))))
