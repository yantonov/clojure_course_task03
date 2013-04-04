(ns clojure-course-task03.dsl.group
  (:require [clojure.string :as s])
  (:use clojure-course-task03.dsl.select))

(defn select-fn-name
  "defines function name to access allowed columns for given group/table"
  [^clojure.lang.Symbol group
   ^clojure.lang.Symbol table]
  (symbol (format "select-%s-%s"
                  (s/lower-case (str group))
                  (s/lower-case (str table)))))

(defn arrow-separator?
  [table-definition]
  (= (second table-definition) '->))

(defn group-privileges-var-name
  [^clojure.lang.Symbol group-name]
  (symbol (format "%s-group-privileges" (str group-name))))

(defn is-all? [columns]
  (and (= 1 (count columns))
       (= ":all" (first columns))))

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
                         allowed (if (is-all? columns)
                                   [:all]
                                   (map keyword columns))
                         fields-var (symbol (str table-name "-fields-var"))]
                     `(defn ~fn-name []
                        (let [~fields-var [~@allowed]]
                          (select ~table-name-sym
                                  (~'fields ~':all))))))
                 privileges)
        ]
    `(list ~group-def ~@fn-defs)))
