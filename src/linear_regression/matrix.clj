(ns linear-regression.matrix)

(defn transpose [m]
  (apply map list m))

(defn v->m [m]
  (if (coll? (first m))
    m
    (list m)) )

(defn mult [m1 m2]
  (cond
    (number? m1) (if (number? m2)
                   (* m1 m2)
                   (for [line m2]
                     (mult m1 line)))
    (number? m2) (mult m2 m1)
    :else (let [mm1 (v->m m1)
                mm2 (v->m m2)
                raw-mult (for [line mm1
                               col  (transpose mm2)]
                           (reduce + (map * line col)))]
            (if (= (count mm1) 1) ;if m1 is a line vector partition will not do what we want
              (list raw-mult)
              (partition (count mm1) raw-mult)))))
