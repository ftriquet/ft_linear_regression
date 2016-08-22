(ns linear-regression.matrix)

(defn transpose [m]
  (apply map list m))

(defn mult [m1 m2]
  (cond
    (number? m1) (if (number? m2)
                   (* m1 m2)
                   (for [line m2]
                     (mult m1 line)))
    (number? m2) (mult m2 m1)
    :else (let [raw-mult (for [line m1
                               col  (transpose m2)]
                           (reduce + (map * line col)))]
            (if (= (count m1) 1) ;if m1 is a line vector partition will not do what we want
              (list raw-mult)
              (partition (count m1) raw-mult)))))
