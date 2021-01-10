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
