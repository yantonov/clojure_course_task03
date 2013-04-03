(ns clojure-course-task03.core
  (:require [clojure.set])
  (:require [clojure-course-task03.dsl
             [group :as group]
             [user :as user]
             [with-user :as with-user]])
  (:use clojure-course-task03.dsl.select))

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
  `(group/group ~name ~@body)
  )

(defmacro user [name & body]
  ;; Пример
  ;; (user Ivanov
  ;;     (belongs-to Agent))
  ;; Создает переменные Ivanov-proposal-fields-var = [:person, :phone, :address, :price]
  ;; и Ivanov-agents-fields-var = [:clients_id, :proposal_id, :agent]
  `(user/user ~name ~@body)
  )

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
  `(with-user/with-user ~name ~@body)
  )
