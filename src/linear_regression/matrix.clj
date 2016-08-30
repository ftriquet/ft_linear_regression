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

(defn mprint [m]
  (cond
    (number? m) (println m)
    (l-vec? m) (println m)
    :else (doseq [l m] (println l))))

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

(defn lines [m]
  (first (dims m)))

(defn cols [m]
  (second (dims m)))

(defn to-line [v]
  (if (l-vec? v)
    v
    (transpose v)))

(defn to-col [v]
  (if (c-vec? v)
    v
    (transpose v)))


(defn mult [m1 m2]
  (cond
    (number? m1) (if (number? m2)
                   (* m1 m2)
                   (map #(mult m1 %) m2))
    (number? m2) (mult m2 m1)
    (not= (lines m2)  (cols m1)) (throw (IllegalArgumentException.
                                          (str "Invalid matrix multiplication: " (lines m1) "x" (cols m1) "*" (lines m2) "x" (cols m2))))
    (l-vec? m1) (if (c-vec? m2)
                  (reduce + (map * m1 (transpose m2)))
                  (for [col (transpose m2)]
                    (reduce + (map * m1 col))))
    (c-vec? m1) (if (l-vec? m2)
                  (for [line m1]
                    (mult (first line) m2))
                  (let [tr (transpose m1)]
                    (for [i m2]
                      (transpose (map #(* i %) tr)))))
    (c-vec? m2) (for [line m1]
                  (list (mult line m2)))
    :else (partition (count m1)
                     (for [line m1 col (transpose m2)]
                       (reduce + (map * line col))))))
(defn sub [m1 m2]
  (cond
    (and (l-vec? m1) (l-vec? m2)) (map - m1 m2)
    (= (dims m1) (dims m2)) (map sub m1 m2)
    :else (throw (IllegalArgumentException.  "Invalid matrix substraction"))))

(defn add [m1 m2]
  (cond
    (and (l-vec? m1) (l-vec? m2)) (map + m1 m2)
    (= (dims m1) (dims m2)) (map add m1 m2)
    :else (throw (IllegalArgumentException.  "Invalid matrix addition"))))

(defn add-bias-feature [inputs]
 (if (l-vec? inputs)
   (cons 1.0 inputs)
   (map add-bias-feature inputs)))

(defn preds [inputs weights]
  (to-line
    (if (l-vec? inputs)
      (reduce + (map * inputs (to-line weights)))
      (mult inputs (to-col weights)))))

(defn abs [x]
  (if (> 0 x) (- x) x))

(defn costs [inputs weights labels f]
  (let [predictions (preds inputs weights)]
    (map #(f  (- %1 %2)) (to-line predictions) (to-line labels))))

(defn cost [inputs weights labels f]
  (/ (float (reduce + (costs inputs weights labels f))) (* 2 (lines inputs))))

