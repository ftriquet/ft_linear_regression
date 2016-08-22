(ns linear-regression.matrix-test
  (:require [clojure.test :refer :all]
            [linear-regression.matrix :refer :all]))

(deftest mult-test
  (is (= 4 (mult 2 2)) "Numbers")

  (is (= [[6 10]
          [8 12]]
         (mult 2 [[3 5] [4 6]])) "Number and matrix")

  (is (= [[6 10]
          [8 12]]
         (mult [[3 5] [4 6]] 2)) "Matrix and number")

  (is (= [2 4 6 8 10] (mult 2 [1 2 3 4 5])) "Number and line vector")

  (is (= [[2] [4] [6] [8] [10]] (mult 2 [[1] [2] [3] [4] [5]])) "Number and column vector")

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

  ;[1 2] should be interpreted the same way than [[1 2]]
  (is (= (mult [[1 2]] [[1 100] [1 100]]) [[3 300]]) "Line vector as a matrix")
  (is (= (mult [1 2] [[1 100] [1 100]]) [[3 300]]) "Line vector as a vector")

  (is (= (mult [[1] [2]] [[1 100]])
         [[1 100]
          [2 200]]) "Column vector")
  )
