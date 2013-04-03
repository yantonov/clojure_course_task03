(ns clojure-course-task03.dsl.group
  (:require [clojure.string :as s])
  (:use clojure-course-task03.dsl.select))

(defn allowed-column-fn-name
  "defines function name to access allowed columns for given group/table"
  [^clojure.lang.Symbol group
   ^clojure.lang.Symbol table]
  (symbol (format "select-%s-%s"
                  (s/lower-case (str group))
                  (s/lower-case (str table)))))

(def group-privileges-registry (ref {}))

(defn register-group!
  "Registers given group in group privileges registry."
  [group-name]
  (dosync
   (alter group-privileges-registry assoc group-name {})))

(defn unregister-group!
  "Unregisters given group"
  [group-name]
  (dosync
   (alter group-privileges-registry dissoc group-name)))

(defn set-group-privileges!
  [group-name privileges]
  (dosync
   (alter group-privileges-registry assoc group-name privileges)))

(defn group-privileges
  "Returns group privileges."
  [group-name]
  (let [reg @group-privileges-registry]
    (reg group-name)))

(defn register-table-privileges!
  "Adds table for given group to group privileges regitry."
  [group-name table-name table-privileges]
  (dosync
   (alter group-privileges-registry
          (fn [reg]
            (let [privileges (group-privileges group-name)]
              (set-group-privileges! group-name
                                     (assoc privileges
                                       table-name
                                       table-privileges)))))))

(defn table-privileges
  "Returns table privileges for given group and table."
  [group-name table-name]
  (let [privileges (group-privileges group-name)]
    (get privileges table-name)))

(defn table-privileges-as-keywords
  "Returns table privileges as vector of keywords."
  [privileges]
  (vec (map keyword privileges)))

(defn arrow-separator?
  [table-definition]
  (= (second table-definition) '->))

(defmacro anaph [sym value & body]
  `(let [~sym ~value]
     ~@body))

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
        group-name-sym (symbol group-name)]
    (register-group! group-name-sym)
    (doseq [table (partition 3 body)]
      (when (arrow-separator? table)
        (let [[table-name _ allowed-columns] table
              table-name-sym (symbol table-name)
              table-var (symbol (str table-name "-fields-var"))
              clmns (vec (map keyword allowed-columns))]
          (register-table-privileges! group-name-sym
                                      table-name-sym
                                      allowed-columns)
          (intern (symbol (ns-name *ns*))
                  (allowed-column-fn-name group-name
                                          table-name)
                  (fn []
                    (eval `(do
                             (use 'clojure-course-task03.dsl.select)
                             (anaph ~table-var ~clmns
                                    (select ~table-name-sym
                                            (~'fields ~':all))))))))))))
