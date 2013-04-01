(ns clojure-course-task03.dsl.with-user-test
  (:require [clojure-course-task03.dsl
             [group :as g]
             [user :as u]
             [with-user :as target]])
  (:require [clojure.test :as test]))

(def test-atom (atom []))

(test/deftest with-user-test
  (test/testing "with-user macro defines local context"
    (g/group Agent
             proposal -> [person, phone, address, price]
             agents -> [client_id, proposal_id, agent])
    (u/user Ivanov
            (belongs-to Agent)
            (reset! test-atom []))
    (target/with-user Ivanov
      (swap! test-atom (fn [a] (conj a proposal-fields-var)))
      (swap! test-atom (fn [a] (conj a agents-fields-var)))
      )
    (let [[a b] @test-atom]
      (test/is (= [:person, :phone, :address, :price]
                  a))
      (test/is (= [:client_id, :proposal_id, :agent]
                  b)))))
