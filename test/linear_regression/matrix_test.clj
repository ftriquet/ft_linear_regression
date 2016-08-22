(ns linear-regression.matrix-test
  (:require [clojure.test :refer :all]
            [linear-regression.matrix :refer :all]))

(deftest mult-test
  (is (= 4 (mult 2 2)))

  (is (= [[4 4 4]
          [4 4 4]
          [4 4 4]]
         (mult 4 [[1 1 1]
                  [1 1 1]
                  [1 1 1]])))

  (is (= [[30 36 42]
          [66 81 96]
          [102 126 150]]
         (mult [[1 2 3]
                [4 5 6]
                [7 8 9]]
               [[1 2 3]
                [4 5 6]
                [7 8 9]])))

  (is (= (mult [[1 2]] [[1 100] [1 100]]) [[3 300]]))

  (is (= (mult [[1]
                [2]]
               [[1 100]]) [[1 100]
                           [2 200]]))
  )
