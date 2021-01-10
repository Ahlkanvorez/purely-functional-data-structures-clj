(ns pfds.ch1-test
  (:require [clojure.test :refer [deftest is]]
            [pfds.ch1 :as ch1]))

(deftest suffixes-test
  (is (= [[1 2 3 4]
          [2 3 4]
          [3 4]
          [4]
          []]
         (ch1/suffixes [1 2 3 4]))))

(deftest better-member-test
  (let [tree (ch1/unbalanced-set
              [(ch1/unbalanced-set
                [(ch1/unbalanced-set [nil 1 nil])
                 2
                 (ch1/unbalanced-set [nil 3 nil])])
               4
               (ch1/unbalanced-set
                [(ch1/unbalanced-set [nil 5 nil])
                 6
                 (ch1/unbalanced-set [nil 7 nil])])])]
    (doseq [n (range 1 8)]
      (is (ch1/member? tree n))

      (is (ch1/better-member? n tree)))))
