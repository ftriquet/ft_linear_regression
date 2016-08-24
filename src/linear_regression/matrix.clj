(ns linear-regression.matrix)

(defn l-vec? [m]
  (and (coll? m) (not (coll? (first m)))))

(defn c-vec? [m]
  (if (not (number? m))
    (let [f (first m)]
      (and (coll? f) (= 1 (count f))))
    false))

(defn matrix? [m]
  (and (not (number? m)) (not (l-vec? m))))


(defn transpose [m]
  (cond
    (number? m) m
    (c-vec? m) (flatten m)
    (l-vec? m) (map list m)
    :else (apply map list m)))

(defn dims [m]
  (cond
    (number? m) 0
    (l-vec? m) (list 1 (count m))
    :else (list (count m) (count (first m)))))


(defn mult [m1 m2]
  (cond
    (number? m1) (if (number? m2)
                   (* m1 m2)
                   (for [line m2]
                     (mult m1 line)))
    (number? m2) (mult m2 m1)
    (not= (first (dims m2)) (second (dims m1))) (throw (IllegalArgumentException.
                                                         "Invalid matrix sizes"))
    (l-vec? m1) (if (c-vec? m2)
                  (reduce + (map * m1 (transpose m2)))
                  (for [col (transpose m2)]
                    (mult m1 (transpose (reverse col)))))
    (c-vec? m1) (if (l-vec? m2)
                  (for [line m1]
                    (mult (first line) m2))
                  (let [tr (transpose m1)]
                    (for [i m2]
                      (transpose (map #(* i %) tr)))))
    :else (partition (count m1)
                     (for [line m1 col (transpose m2)]
                       (reduce + (map * line col))))))
