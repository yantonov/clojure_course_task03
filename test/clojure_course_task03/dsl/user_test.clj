(ns clojure-course-task03.dsl.user-test
  (:require [clojure.test :as test])
  (:require [clojure-course-task03.dsl.group :as g])
  (:require [clojure-course-task03.dsl.user :as target]))

(test/deftest var-name-which-holds-table-privileges-test
  (test/testing "Name convention for variable names"
    (test/is (= 'Ivanov-proposal-fields-var
                (target/var-name-which-holds-table-privileges 'Ivanov 'proposal)))))

(test/deftest user-macro-defines-variables-contained-table-privileges
  (test/testing "User macro defines variables contained table privileges."
    (let [ns-sym 'clojure-course-task03.dsl.user-test]
      (g/group Agent
               proposal -> [person, phone, address, price]
               agents -> [client_id, proposal_id, agent])
      (target/user Ivanov
                   (belongs-to Agent))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-proposal-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-agents-fields-var))))))
  )

(test/deftest variables-defined-by-user-macro-contains-table-privileges
  (test/testing "Variables defined by user macro contains table privileges."
    (let [ns-sym 'clojure-course-task03.dsl.user-test]
      (g/group Agent
               proposal -> [person, phone, address, price]
               agents -> [client_id, proposal_id, agent])
      (target/user Ivanov
                   (belongs-to Agent))
      (test/is (= [:person, :phone, :address, :price]
                  @(ns-resolve ns-sym 'Ivanov-proposal-fields-var)))
      (test/is (= [:client_id, :proposal_id, :agent]
                  @(ns-resolve ns-sym 'Ivanov-agents-fields-var))))))


(test/deftest user-macro-updates-user-tables-var-collection
  (g/group Agent
           proposal -> [person, phone, address, price]
           agents -> [client_id, proposal_id, agent])
  (target/user Ivanov
               (belongs-to Agent))
  (let [vars (target/get-user-tables-vars)]
    (test/is (contains? vars 'Ivanov-agents-fields-var))
    (test/is (contains? vars 'Ivanov-proposal-fields-var))
    )
  )

(test/deftest attach-to-multiple-groups
  (test/testing "user macro can be used to attach user to multiple groups"
    (let [ns-sym 'clojure-course-task03.dsl.user-test]
      (g/group A
               a_table -> [a_column])
      (g/group B
               b_table -> [b_column])
      (g/group C
               c_table -> [c_column])
      (target/user Ivanov
                   (belongs-to A B C))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-a_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-b_table-fields-var))))
      (test/is (false? (nil? (ns-resolve ns-sym
                                         'Ivanov-c_table-fields-var))))
      )))
