(ns clojure-course-task03.user-test
  (:require [clojure.test :as test])
  (:use clojure-course-task03.core))

(test/deftest var-name-which-holds-table-privileges-test
  (test/testing "Name convention for variable names"
    (test/is (= 'Ivanov-proposal-fields-var
                (table-privileges-var-name 'Ivanov 'proposal)))))

(group
 Agent
 proposal -> [person, phone, address, price]
 agents -> [client_id, proposal_id, agent])

(user
 Ivanov
 (belongs-to Agent))

(test/deftest user-macro-defines-variables-contained-table-privileges
  (test/testing "User macro defines variables contained table privileges."
    (let [ns-sym 'clojure-course-task03.user-test]
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-proposal-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-agents-fields-var)))))))


(group
 Agent1
 proposal -> [person, phone, address, price]
 agents -> [client_id, proposal_id, agent])

(user
 Ivanov1
 (belongs-to Agent1))

(test/deftest variables-defined-by-user-macro-contains-table-privileges
  (test/testing "Variables defined by user macro contains table privileges."
    (let [ns-sym 'clojure-course-task03.user-test]
      (test/is (= [:person, :phone, :address, :price]
                  @(ns-resolve ns-sym 'Ivanov1-proposal-fields-var)))
      (test/is (= [:client_id, :proposal_id, :agent]
                  @(ns-resolve ns-sym 'Ivanov1-agents-fields-var))))))


(group
 A
 a_table -> [a_column])

(group
 B
 b_table -> [b_column])

(group
 C
 c_table -> [c_column])

(user
 Ivanov2
 (belongs-to A B C))

(test/deftest attach-to-multiple-groups
  (test/testing "user macro can be used to attach user to multiple groups"
    (let [ns-sym 'clojure-course-task03.user-test]
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov2-a_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov2-b_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov2-c_table-fields-var)))))))

(group
 D
 d_table -> [d_column])

(group
 E
 e_table -> [e_column])

(user
 Ivanov3
 (belongs-to E)
 (belongs-to D))

(test/deftest multiple-belongs-to-statements
  (test/testing "user macro can contain multiple belongs to command"
    (let [ns-sym 'clojure-course-task03.user-test]
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov3-d_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov3-e_table-fields-var)))))))


(group
 F
 f_table -> [f1_column])

(group
 G
 f_table -> [f2_column])

(user
 Ivanov4
 (belongs-to F G))

(test/deftest union-group-privileges-for-same-table
  (test/testing "union group privileges for same table"
    (let [ns-sym 'clojure-course-task03.user-test]
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov4-f_table-fields-var))))
      (test/is (= [:f1_column :f2_column] @(ns-resolve ns-sym
                                  'Ivanov4-f_table-fields-var))))))
