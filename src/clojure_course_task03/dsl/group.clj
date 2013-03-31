(ns clojure-course-task03.dsl.group)

(defn allowed-column-fn-name
  "defines function name to access allowed columns for given group/table"
  [^clojure.lang.Symbol group
   ^clojure.lang.Symbol table]
  (symbol (str "select"
               "-"
               (.toLowerCase (str group))
               "-"
               (.toLowerCase (str table)))))

(def group-privileges-registry (ref {}))

(defn register-group!
  "Registers given group in group privileges registry."
  [group-name]
  (dosync
   (alter group-privileges-registry
          (fn [reg] (assoc reg group-name {})))))

(defn unregister-group!
  "Unregisters given group"
  [group-name]
  (dosync
   (alter group-privileges-registry
          (fn [reg] (dissoc reg group-name))))
  )

(defn set-group-privileges!
  [group-name privileges]
  (dosync
   (alter group-privileges-registry
          (fn [reg] (assoc reg group-name privileges)))))

(defn group-privileges
  "Returns group privileges."
  [group-name]
  (@group-privileges-registry group-name))

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
    (get privileges table-name))
  )

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
        group-name-sym (symbol group-name)
        table-seq (partition 3 body)
        defn-seq (for [table table-seq]
                   (let [[table-name _ table-privileges] table
                         fn-name (allowed-column-fn-name group-name
                                                         table-name)
                         table-name-sym (symbol table-name)]
                     `(do
                        (register-table-privileges! '~group-name-sym
                                                    '~table-name-sym
                                                    '~table-privileges)
                        (defn ~fn-name [] (table-privileges '~group-name-sym
                                                            '~table-name-sym)))))]
    `(do
       (register-group! '~group-name-sym)
       ~@defn-seq
       nil)
    ))