(ns linear-regression.learning-test
  (:require [clojure.test :refer :all]
            [linear-regression.learning :refer :all]))

(deftest scale-test
  (is (= (scale [[1.0 13.0 3.0 2.0]
                 [13.0 4.0 8.0 6.0]
                 [25.0 28.0 6.0 6.0]])
         [[0.0 0.375 0.0 0.0]
          [0.5 0.0 1.0 1.0]
          [1.0 1.0 0.6 1.0]]))

  (is (= (scale [[2.0 26.0 6.0 4.0]
                 [26.0 8.0 16.0 12.0]
                 [50.0 56.0 12.0 12.0]])
         [[0.0 0.375 0.0 0.0]
          [0.5 0.0 1.0 1.0]
          [1.0 1.0 0.6 1.0]]))

  )
