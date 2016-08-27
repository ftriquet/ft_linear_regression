(ns linear-regression.learning
  (:require [linear-regression.matrix :refer :all]
            [quil.middleware :as m]
            [quil.core :as q]))

(defn cs [x]
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

(defn mean-scale [inputs]
  (f-scale inputs #(/ (reduce + %) (count %))))

(defn new-weights [inputs weights labels lrn-rate f]
  (sub weights
       (mult (/ lrn-rate (double (lines inputs)))
             (mult (costs inputs weights labels f) inputs))))

(defn init-learning [thetas X error labels]
  (fn [] (q/frame-rate 10)
    (q/color-mode :hsb)
    (q/background 240)
    {:X X
     :error error
     :thetas thetas
     :labels labels
     :errors [[0 error]]}))

(defn update-state [{X :X thetas :thetas error :error labels :labels errors :errors :as state}]
  (println)
  (let [new-error (cost X thetas labels cs)]
    (println "Current error:" error)
    (println "Thetas: " thetas)
    {:X X
     :thetas (new-weights X thetas labels 0.001 cs)
     :error new-error
     :labels labels
     :errors (conj  errors [(inc (first (last errors))) new-error])}))

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

(defn train [inputs labels]
  ;(let [thetas  (zeros (inc (cols inputs)))
  (let [thetas  (zeros (inc (cols inputs)))
        X       (add-bias-feature (scale inputs))
        error   (cost X thetas labels cs)]
    (q/defsketch linear-regression
      :title "Linear regression"
      :size [800 800]
      :setup (init-learning thetas X error labels)
      :update update-state
      :draw draw-points
      :middleware [m/fun-mode])))

(defn new-weights [inputs weights labels lrn-rate f]
  (sub weights
       (mult (/ lrn-rate (double (lines inputs)))
             (mult (costs inputs weights labels f) inputs))))


(defn compute-costs [X theta labels]
  (sub (mult X (to-col theta)) labels))



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

(defn get-ext [inputs]
  (let [tr (transpose inputs)]
    (if (l-vec? tr)
      (list (list (apply min tr)) (list (apply max tr)))
      (list (map #(apply min %) tr) (map #(apply max %) tr)))))


(defn train-model [inputs labels lrn-rate]
  (let [X (add-bias-feature (scale inputs))
        weights (zeros (inc (cols inputs)))
       [minsX maxsX] (get-ext inputs)]
    (println "Start computing, actual thetas: " weights)
    (loop [theta weights
           error (cost X theta labels #(* % %))]
      (let [d-theta (mult (/ lrn-rate (double (lines X)))
                          (mult (to-line (compute-costs X theta labels)) X))
            new-theta (sub theta d-theta)
            err (cost X new-theta labels #(* % %))]
        (if (< (- error err) 0.00001)
          (do
            (println "Done! Thetas: " new-theta)
            [["mins" "maxs" "theta"]
             [(pr-str minsX) (pr-str maxsX) (pr-str new-theta)]])
          (recur new-theta err))))))
