(ns clojure-course-task03.dsl.user-test
  (:require [clojure.test :as test])
  (:require [clojure-course-task03.dsl
             [group :as g]
             [user :as target]]))

(test/deftest var-name-which-holds-table-privileges-test
  (test/testing "Name convention for variable names"
    (test/is (= 'Ivanov-proposal-fields-var
                (target/table-privileges-var-name 'Ivanov 'proposal)))))

(g/group
 Agent
 proposal -> [person, phone, address, price]
 agents -> [client_id, proposal_id, agent])

(target/user
 Ivanov
 (belongs-to Agent))

(test/deftest user-macro-defines-variables-contained-table-privileges
  (test/testing "User macro defines variables contained table privileges."
    (let [ns-sym 'clojure-course-task03.dsl.user-test]
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-proposal-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-agents-fields-var)))))))


(g/group
 Agent1
 proposal -> [person, phone, address, price]
 agents -> [client_id, proposal_id, agent])

(target/user
 Ivanov1
 (belongs-to Agent1))

(test/deftest variables-defined-by-user-macro-contains-table-privileges
  (test/testing "Variables defined by user macro contains table privileges."
    (let [ns-sym 'clojure-course-task03.dsl.user-test]
      (test/is (= [:person, :phone, :address, :price]
                  @(ns-resolve ns-sym 'Ivanov1-proposal-fields-var)))
      (test/is (= [:client_id, :proposal_id, :agent]
                  @(ns-resolve ns-sym 'Ivanov1-agents-fields-var))))))


(g/group
 A
 a_table -> [a_column])

(g/group
 B
 b_table -> [b_column])

(g/group
 C
 c_table -> [c_column])

(target/user
 Ivanov2
 (belongs-to A B C))

(test/deftest attach-to-multiple-groups
  (test/testing "user macro can be used to attach user to multiple groups"
    (let [ns-sym 'clojure-course-task03.dsl.user-test]
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov2-a_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov2-b_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov2-c_table-fields-var)))))))

(g/group
 D
 d_table -> [d_column])

(g/group
 E
 e_table -> [e_column])

(target/user
 Ivanov3
 (belongs-to E)
 (belongs-to D))

(test/deftest multiple-belongs-to-statements
  (test/testing "user macro can contain multiple belongs to command"
    (let [ns-sym 'clojure-course-task03.dsl.user-test]
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov3-d_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov3-e_table-fields-var)))))))


(g/group
 F
 f_table -> [f1_column])

(g/group
 G
 f_table -> [f2_column])

(target/user
 Ivanov4
 (belongs-to F G))

(test/deftest union-group-privileges-for-same-table
  (test/testing "union group privileges for same table"
    (let [ns-sym 'clojure-course-task03.dsl.user-test]
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov4-f_table-fields-var))))
      (test/is (= [:f1_column :f2_column] @(ns-resolve ns-sym
                                  'Ivanov4-f_table-fields-var))))))
