(ns clojure-course-task03.with-user-test
  (:require [clojure.test :as test])
  (:use [clojure-course-task03.core]))

(def test-atom (atom []))

(test/deftest with-user-test
  (test/testing "with-user macro defines local context"
    (eval '(clojure-course-task03.core/group
            Agent
            proposal -> [person, phone, address, price]
            agents -> [client_id, proposal_id, agent]))
    (eval '(clojure-course-task03.core/user
            Ivanov
            (belongs-to Agent)
            (reset! clojure-course-task03.with-user-test/test-atom [])))
    (eval '(clojure-course-task03.core/with-user
             Ivanov
             (swap! clojure-course-task03.with-user-test/test-atom
                    (fn [a] (conj a proposal-fields-var)))
             (swap! clojure-course-task03.with-user-test/test-atom
                    (fn [a] (conj a agents-fields-var)))))
    (let [[a b] @test-atom]
      (test/is (= [:person, :phone, :address, :price]
                  a))
      (test/is (= [:client_id, :proposal_id, :agent]
                  b)))))
