(ns clojure-course-task03.dsl.with-user-test
  (:require [clojure-course-task03.dsl
             [group :as g]
             [user :as u]
             [with-user :as target]])
  (:require [clojure.test :as test]))

(def test-atom (atom []))

(g/group
 Agent
 proposal -> [person, phone, address, price]
 agents -> [client_id, proposal_id, agent])

(u/user
 Ivanov
 (belongs-to Agent)
 (reset! clojure-course-task03.dsl.with-user-test/test-atom []))

(target/with-user
  Ivanov
  (swap! clojure-course-task03.dsl.with-user-test/test-atom
         (fn [a] (conj a proposal-fields-var)))
  (swap! clojure-course-task03.dsl.with-user-test/test-atom
         (fn [a] (conj a agents-fields-var))))

(test/deftest with-user-test
  (test/testing "with-user macro defines local context"
    (let [[a b] @test-atom]
      (test/is (= [:person, :phone, :address, :price]
                  a))
      (test/is (= [:client_id, :proposal_id, :agent]
                  b)))))
