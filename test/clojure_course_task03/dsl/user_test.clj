(ns clojure-course-task03.dsl.user-test
  (:require [clojure.test :as test])
  (:require [clojure-course-task03.dsl
             [group :as g]
             [user :as target]]))

(test/deftest var-name-which-holds-table-privileges-test
  (test/testing "Name convention for variable names"
    (test/is (= 'Ivanov-proposal-fields-var
                (target/table-privileges-var-name 'Ivanov 'proposal)))))

(test/deftest user-macro-defines-variables-contained-table-privileges
  (test/testing "User macro defines variables contained table privileges."
    (let [ns-sym (symbol (ns-name *ns*))]
      (eval '(clojure-course-task03.dsl.group/group
              Agent
              proposal -> [person, phone, address, price]
              agents -> [client_id, proposal_id, agent]))
      (eval '(clojure-course-task03.dsl.user/user
              Ivanov
              (belongs-to Agent)))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-proposal-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-agents-fields-var)))))))

(test/deftest variables-defined-by-user-macro-contains-table-privileges
  (test/testing "Variables defined by user macro contains table privileges."
    (let [ns-sym (symbol (ns-name *ns*))]
      (eval '(clojure-course-task03.dsl.group/group
              Agent
              proposal -> [person, phone, address, price]
              agents -> [client_id, proposal_id, agent]))
      (eval '(clojure-course-task03.dsl.user/user
              Ivanov
              (belongs-to Agent)))
      (test/is (= [:person, :phone, :address, :price]
                  @(ns-resolve ns-sym 'Ivanov-proposal-fields-var)))
      (test/is (= [:client_id, :proposal_id, :agent]
                  @(ns-resolve ns-sym 'Ivanov-agents-fields-var))))))


(test/deftest user-macro-updates-user-tables-var-collection
  (eval '(clojure-course-task03.dsl.group/group
          Agent
          proposal -> [person, phone, address, price]
          agents -> [client_id, proposal_id, agent]))
  (eval '(clojure-course-task03.dsl.user/user
          Ivanov
          (belongs-to Agent)))
  (let [vars (target/get-user-tables-vars)]
    (test/is (contains? vars 'Ivanov-agents-fields-var))
    (test/is (contains? vars 'Ivanov-proposal-fields-var))))

(test/deftest attach-to-multiple-groups
  (test/testing "user macro can be used to attach user to multiple groups"
    (let [ns-sym (symbol (ns-name *ns*))]
      (eval '(clojure-course-task03.dsl.group/group
              A
              a_table -> [a_column]))
      (eval '(clojure-course-task03.dsl.group/group
              B
              b_table -> [b_column]))
      (eval '(clojure-course-task03.dsl.group/group
              C
              c_table -> [c_column]))
      (eval '(clojure-course-task03.dsl.user/user
              Ivanov
              (belongs-to A B C)))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-a_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-b_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-c_table-fields-var)))))))

(test/deftest multiple-belongs-to-statements
  (test/testing "user macro can be used to attach user to multiple groups"
    (let [ns-sym (symbol (ns-name *ns*))]
      (eval '(clojure-course-task03.dsl.group/group
              D
              d_table -> [d_column]))
      (eval '(clojure-course-task03.dsl.group/group
              E
              e_table -> [e_column]))
      (eval '(clojure-course-task03.dsl.user/user
              Ivanov3
              (belongs-to D)
              (belongs-to E)))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov3-d_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov3-e_table-fields-var)))))))

(test/deftest union-group-privileges-for-same-table
  (test/testing "user macro can be used to attach user to multiple groups"
    (let [ns-sym (symbol (ns-name *ns*))]
      (eval '(clojure-course-task03.dsl.group/group
              F
              f_table -> [f1_column]))
      (eval '(clojure-course-task03.dsl.group/group
              G
              f_table -> [f2_column]))
      (eval '(clojure-course-task03.dsl.user/user
              Ivanov4
              (belongs-to F G)))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov4-f_table-fields-var))))
      (test/is (= [:f1_column :f2_column]
                  @(ns-resolve ns-sym
                               'Ivanov4-f_table-fields-var))))))
