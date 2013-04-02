(ns clojure-course-task03.dsl.group
  (:require [clojure.string :as s]))

(defn select-fn-name
  "defines function name to access allowed columns for given group/table"
  [^clojure.lang.Symbol group
   ^clojure.lang.Symbol table]
  (symbol (str "select"
               "-"
               (s/lower-case (str group))
               "-"
               (s/lower-case (str table)))))

(defn select-all?
  [columns]
  (and (= 1 (count columns))
       (= ":all" (str (first columns)))))

(defn sql-statement
  [table-name columns]
  (if (select-all? columns)
    (format "SELECT * FROM %s " table-name)
    (format "SELECT %s FROM %s "
            (clojure.string/join ","
                                 (map str columns))
            table-name)))

(defn arrow-separator?
  [table-definition]
  (= (second table-definition) '->))

(defn group-privileges-var-name
  [^clojure.lang.Symbol group-name]
  (symbol (str (str group-name)
               "-"
               "group"
               "-"
               "privileges")))

(defmacro group [name & body]
  ;; Sample
  ;; (group Agent
  ;;      proposal -> [person, phone, address, price]
  ;;      agents -> [clients_id, proposal_id, agent])
  ;; 1) Create group named Agent
  ;; 2) Memorize allowed tables and columns for given group
  ;; 3) Create functions to retrieve table privileges
  ;;    (select-agent-proposal) ;; select person, phone, address, price from proposal;
  ;;    (select-agent-agents)  ;; select clients_id, proposal_id, agent from agents;
  (let [group-name name
        privileges-var-name (group-privileges-var-name group-name)
        privileges (into {}
                         (for [table (partition 3 body)]
                           (when (arrow-separator? table)
                             (let [[table-name _ allowed-columns] table]
                               [(str table-name)
                                (vec (map str allowed-columns))]))))
        group-def `(def ~privileges-var-name ~privileges)
        fn-defs (map
                 (fn [[table-name columns]]
                   (let [table-name-sym (symbol table-name)
                         fn-name (select-fn-name group-name
                                                 table-name-sym)
                         select-stmt (sql-statement table-name-sym
                                                    (map symbol columns))]
                     `(defn ~fn-name [] ~select-stmt)))
                 privileges)
        ]
    `(list ~group-def ~@fn-defs)))
