(ns clojure-course-task03.core
  (:require [clojure.set])
  (:require [clojure.string :as s]))

(defn join* [table-name conds]
  (let [op (first conds)
        f1 (name (nth conds 1))
        f2 (name (nth conds 2))]
    (str table-name " ON " f1 " " op " " f2)))

(defn where* [data]
  (let [ks (keys data)
        res (reduce str (doall (map #(let [src (get data %)
                                           v (if (string? src)
                                               (str "'" src "'")
                                               src)]
                                       (str (name %) " = " v ",")) ks)))]
    (reduce str (butlast res))))

(defn order* [column ord]
  (str (name column)
       (if-not (nil? ord) (str " " (name ord)))))

(defn limit* [v] v)

(defn offset* [v] v)

(defn -fields** [data]
  (reduce str (butlast (reduce str (doall (map #(str (name %) ",") data))))))

(defn fields* [flds allowed]
  (let [v1 (apply hash-set flds)
        v2 (apply hash-set allowed)
        v (clojure.set/intersection v1 v2)]
    (cond
     (and (= (first flds) :all) (= (first allowed) :all)) "*"
     (and (= (first flds) :all) (not= (first allowed) :all)) (-fields** allowed)
     (= :all (first allowed)) (-fields** flds)
     :else (-fields** (filter v flds)))))

(defn select* [table-name {:keys [fields where join order limit offset]}]
  (-> (str "SELECT " fields " FROM " table-name " ")
      (str (if-not (nil? where) (str " WHERE " where)))
      (str (if-not (nil? join) (str " JOIN " join)))
      (str (if-not (nil? order) (str " ORDER BY " order)))
      (str (if-not (nil? limit) (str " LIMIT " limit)))
      (str (if-not (nil? offset) (str " OFFSET " offset)))))


(defmacro select [table-name & data]
  (let [;; Var containing allowed fields
        fields-var# (symbol (str table-name "-fields-var"))

        ;; The function takes one of the definitions like (where ...) or (join ...)
        ;; and returns a map item [:where (where* ...)] or [:join (join* ...)].
        transf (fn [elem]
                 (let [v (first elem)
                       v2 (second elem)
                       v3 (if (> (count elem) 2) (nth elem 2) nil)
                       val (case v
                             fields (list 'fields* (vec (next elem)) fields-var#)
                             offset (list 'offset* v2)
                             limit (list 'limit* v2)
                             order (list 'order* v2 v3)
                             join (list 'join* (list 'quote v2) (list 'quote v3))
                             where (list 'where* v2))]
                   [(keyword v) val]))

        ;; Takes a list of definitions like '((where ...) (join ...) ...) and returns
        ;; a vector [[:where (where* ...)] [:join (join* ...)] ...].
        env* (loop [d data
                    v (first d)
                    res []]
               (if (empty? d)
                 res
                 (recur (next d) (second d) (conj res (transf v)))))

        ;; Accepts vector [[:where (where* ...)] [:join (join* ...)] ...],
        ;; returns map {:where (where* ...), :join (join* ...), ...}
        env# (apply hash-map (apply concat env*))]

    `(select* ~(str table-name)  ~env#)))


;; Examples:
;; -------------------------------------

(let [proposal-fields-var [:person, :phone, :address, :price]]
  (select proposal
          (fields :person, :phone, :id)
          (where {:price 11})
          (join agents (= agents.proposal_id proposal.id))
          (order :f3)
          (limit 5)
          (offset 5)))

(let [proposal-fields-var [:person, :phone, :address, :price]]
  (select proposal
          (fields :all)
          (where {:price 11})
          (join agents (= agents.proposal_id proposal.id))
          (order :f3)
          (limit 5)
          (offset 5)))

(let [proposal-fields-var [:all]]
  (select proposal
          (fields :all)
          (where {:price 11})
          (join agents (= agents.proposal_id proposal.id))
          (order :f3)
          (limit 5)
          (offset 5)))


(comment
  ;; Описание и примеры использования DSL
  ;; ------------------------------------
  ;; Предметная область -- разграничение прав доступа на таблицы в реелтерском агенстве
  ;;
  ;; Работают три типа сотрудников: директор (имеет доступ ко всему), операторы ПК (принимают заказы, отвечают на тел. звонки,
  ;; передают агенту инфу о клиентах), агенты (люди, которые лично встречаются с клиентами).
  ;;
  ;; Таблицы:
  ;; proposal -> [id, person, phone, address, region, comments, price]
  ;; clients -> [id, person, phone, region, comments, price_from, price_to]
  ;; agents -> [proposal_id, agent, done]

  ;; Определяем группы пользователей и
  ;; их права на таблицы и колонки
  (group Agent
         proposal -> [person, phone, address, price]
         agents -> [clients_id, proposal_id, agent])

  ;; Предыдущий макрос создает эти функции
  (select-agent-proposal) ;; select person, phone, address, price from proposal;
  (select-agent-agents)  ;; select clients_id, proposal_id, agent from agents;




  (group Operator
         proposal -> [:all]
         clients -> [:all])

  ;; Предыдущий макрос создает эти функции
  (select-operator-proposal) ;; select * proposal;
  (select-operator-clients)  ;; select * from clients;



  (group Director
         proposal -> [:all]
         clients -> [:all]
         agents -> [:all])

  ;; Предыдущий макрос создает эти функции
  (select-director-proposal) ;; select * proposal;
  (select-director-clients)  ;; select * from clients;
  (select-director-agents)  ;; select * from agents;


  ;; Определяем пользователей и их группы

  (user Ivanov
        (belongs-to Agent))

  (user Sidorov
        (belongs-to Agent))

  (user Petrov
        (belongs-to Operator))

  (user Directorov
        (belongs-to Operator,
                    Agent,
                    Director))


  ;; Оператор select использует внутри себя переменную <table-name>-fields-var.
  ;; Для указанного юзера макрос with-user должен определять переменную <table-name>-fields-var
  ;; для каждой таблицы, которая должна содержать список допустимых полей этой таблицы
  ;; для этого пользователя.

  ;; Агенту можно видеть свои "предложения"
  (with-user Ivanov
    (select proposal
            (fields :person, :phone, :address, :price)
            (join agents (= agents.proposal_id proposal.id))))

  ;; Агенту не доступны клиенты
  (with-user Ivanov
    (select clients
            (fields :all)))  ;; Empty set

  ;; Директор может видеть состояние задач агентов
  (with-user Directorov
    (select agents
            (fields :done)
            (where {:agent "Ivanov"})
            (order :done :ASC)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TBD: Implement the following macros
;;

;;; group

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
  ;; Пример
  ;; (group Agent
  ;;      proposal -> [person, phone, address, price]
  ;;      agents -> [clients_id, proposal_id, agent])
  ;; 1) Создает группу Agent
  ;; 2) Запоминает, какие таблицы (и какие колонки в таблицах)
  ;;    разрешены в данной группе.
  ;; 3) Создает следующие функции
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
                             (use 'clojure-course-task03.core)
                             (anaph ~table-var ~clmns
                                    (select ~table-name-sym
                                            (~'fields ~':all))))))))))))

;;; group

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
  ;; Пример
  ;; (user Ivanov
  ;;     (belongs-to Agent))
  ;; Создает переменные Ivanov-proposal-fields-var = [:person, :phone, :address, :price]
  ;; и Ivanov-agents-fields-var = [:clients_id, :proposal_id, :agent]
  (let [user-name name
        security-items (get-security-items body)
        defined-var-values (atom {})
        ns-sym (symbol (ns-name *ns*))]
    (doseq [group-name (mapcat rest security-items)]
      (doseq [table-name (keys (group-privileges group-name))]
        (let [var-name-to-def (table-privileges-var-name user-name
                                                         table-name)
              new-val (table-privileges-as-keywords
                       (table-privileges group-name
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

;;; user end


;;; with-user

(defn value-for-defined-var
  [^clojure.lang.Symbol variable]
  (let [v (ns-resolve (symbol (ns-name *ns*))
                      variable)]
    (if (nil? v) nil @v)))

(defmacro with-user [name & body]
  ;; Пример
  ;; (with-user Ivanov
  ;;   . . .)
  ;; 1) Находит все переменные, начинающиеся со слова Ivanov, в *user-tables-var*s
  ;;    (Ivanov-proposal-fields-var и Ivanov-agents-fields-var)
  ;; 2) Создает локальные привязки без префикса Ivanov-:
  ;;    proposal-fields-var и agents-fields-var.
  ;;    Таким образом, функция select, вызванная внутри with-user, получает
  ;;    доступ ко всем необходимым переменным вида <table-name>-fields-var.

  (let [user-name (str name)
        strip-prefix-len (inc (.length user-name))]
    (let [bindings (apply vector (apply concat
                                        (for [v (get-user-tables-vars)
                                              :when (.startsWith (str v) user-name)]
                                          [(symbol (.substring (str v)
                                                               strip-prefix-len))
                                           (value-for-defined-var v)])))]
      `(let ~bindings ~@body))))
