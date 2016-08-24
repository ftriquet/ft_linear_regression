(ns linear-regression.matrix-test
  (:require [clojure.test :refer :all]
            [linear-regression.matrix :refer :all]))

(deftest l-vec?-test
  (is (l-vec? [1 2 3 4]) "Line vector")
  (is (= false (l-vec? 1)) "Number")
  (is (= false (l-vec? [[1] [2]])) "Column vector")
  (is (= false (l-vec? [[1 2 3] [4 5 6] [7 8 9]])) "Matrix"))

(deftest c-vec?-test

  (is (= false (c-vec? [1 2 3 4])) "Line vector")
  (is (= false (c-vec? 1)) "Number")
  (is (= true  (c-vec? [[1] [2]])) "Column vector")
  (is (= false (c-vec? [[1 2 3] [4 5 6] [7 8 9]])) "Matrix"))


(deftest matrix?-test
  (is (= true (matrix? [[1 4] [2 5] [3 6]])) "Matrix")
  (is (= false (matrix? 8)) "Number")
  (is (= false (matrix? [1 2 3 4])) "Vector")
  (is (= true (matrix? [[1] [2] [3]])) "Column vector"))

(deftest dims-test
  (is (= '(1 2) (dims [4 5])))
  (is (= '(3 4) (dims [[1 2 3 4] [1 2 3 4] [1 2 3 4]])))
  )


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

  (is (= [[30 36 42]
          [66 81 96]
          [102 126 150]]
         (mult [[1 2 3]
                [4 5 6]
                [7 8 9]]
               [[1 2 3]
                [4 5 6]
                [7 8 9]])) "Two square matrices")

  (is (= (mult [1 2] [[1 100] [1 100]]) [3 300]) "Line by matrix")
  (is (= 30 (mult [1 2 3 4] [[1] [2] [3] [4]])) "Line by column")
  (is (= (mult [[1] [2]] [1 100])
         [[1 100]
          [2 200]]) "Column by line")

  )

(deftest transpose-test
  (is (= (transpose [[1 2 3 4]]) [[1] [2] [3] [4]]) "Line vector")
  (is (= (transpose [[1 2 3]
                     [4 5 6]]) [[1 4]
                                [2 5]
                                [3 6]]) "Matrix")
  (is (= (transpose [[1] [2] [3] [4]]) [1 2 3 4]) "Column vector"))

(comment

  (deftest predict-test
    (is (= 60 (predict [[1 3 2 5]] [[3 5 8]])))
    ))
