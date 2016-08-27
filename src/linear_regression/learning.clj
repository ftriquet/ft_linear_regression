(ns linear-regression.learning
  (:require [linear-regression.matrix :refer :all]
            [quil.core :as q]))

(defn sq [x]
  (* x x))

(defn zeros [n]
  (take n (repeat 0.0)))

(defn f-range [v]
  (-  (apply max v) (apply min v)))

(defn f-scale [inputs f]
  (if (not (c-vec? inputs))
    (transpose (for [col (transpose inputs)]
                 (let [rng (f-range col)
                       a (f col)]
                   (map #(/ (- % a) rng) col))))
    (let [l (transpose inputs)
          rng (f-range l)
          a (f l)]
      (transpose (map #(/ (- % a) rng) l)))))

(defn scale [inputs]
  (f-scale inputs (partial apply min)))


(defn draw-error [{X :X thetas :thetas errors :errors}]
  (doseq [[i e] errors]
    (when (> e 0)
      (q/point i (- 800 (int (/ (* 700 e)  (apply max (map second errors))))))
      (q/point (inc i) (- 800 (int (/ (* 700 e)  (apply max (map second errors))))))
      (q/point i (inc (- 800 (int (/ (* 700 e)  (apply max (map second errors)))))))
      (q/point (inc i) (inc (- 800 (int (/ (* 700 e)  (apply max (map second errors))))))))))

(defn draw-points [{X :X labels :labels}]
  (q/background 255)
  (doseq [[x [y]] (partition 2 (interleave (map second X) labels))]
    (q/rect (* 800.0 x)
            (- 800 (* 800.0 (/ (double y) (apply max (map first labels)))))
            4 4)))

(defn compute-costs [X theta labels]
  (sub (mult X (to-col theta)) labels))


(defn get-ext [inputs]
  (let [tr (transpose inputs)]
    (if (l-vec? tr)
      (list (list (apply min tr)) (list (apply max tr)))
      (list (map #(apply min %) tr) (map #(apply max %) tr)))))

(defn init-draw [{X :X theta :theta labels :labels minx :min maxx :max :as state}]
  (fn []
    (q/frame-rate 1)
    (q/background 240)
    {:X X
     :min minx
     :max maxx
     :theta theta
     :labels labels}))

(defn upd [state] state)

(defn draw-result [{X :X theta :theta labels :labels minx :min maxx :max :as state}]
  (q/background 255)
  (doseq [[x [y]] (partition 2 (interleave (map second X) labels))]
    (q/rect (* 800.0 x)
            (- 800 (* 800.0 (/ (double y) (apply max (map first labels)))))
            4 4))
  (let [x1 0.0
        y1 (+ (first theta) (* x1 (second theta)))
        x2 1.0
        y2 (+ (first theta) (* x2 (second theta)))]
    (q/line
      (* 800.0 x1)
      (- 800 (* 800.0 (/ (double y1) (apply max (map first labels)))))
      (* 800.0 x2)
      (- 800 (* 800.0 (/ (double y2) (apply max (map first labels))))))))


(defn train-model [inputs labels lrn-rate]
  (let [X (add-bias-feature (scale inputs))
        weights (zeros (inc (cols inputs)))
        [minsX maxsX] (get-ext inputs)]
    (println "Start computing, actual thetas: " weights)
    (loop [theta weights
           error (cost X theta labels sq)]
      (let [d-theta (mult (/ lrn-rate (double (lines X)))
                          (mult (to-line (compute-costs X theta labels)) X))
            new-theta (sub theta d-theta)
            err (cost X new-theta labels sq)]
        (if (< (- error err) 0.000001)
          (do
            (println "Done! Thetas: " new-theta)
            {:csv-data [["mins" "maxs" "theta"]
                        [(pr-str minsX) (pr-str maxsX) (pr-str new-theta)]]
             :X X
             :theta new-theta
             :labels labels
             :min minsX
             :max maxsX})
          (recur new-theta err))))))
