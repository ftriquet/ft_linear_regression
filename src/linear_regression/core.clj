(ns linear-regression.core
  (:gen-class)
  (:require [linear-regression.matrix :refer :all]
            [linear-regression.learning :refer :all]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.tools.cli :as cli]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure-csv.core :as csv]))

(def cli-options [["-t" "--train"]
                  ["-p" "--predict"]
                  ["-g" "--graph"]
                  ["-a" "--animation"]
                  ["-f" "--file FILE" "data file" :default "resources/data.csv"]])

(defn- check-data [data]
  (let [s (count (first data))]
    (when (every? #(= (count %) s) data) true)))

(defn- format-data [data]
  (let [to-f (map #(map (comp float read-string) %) data)]
    (list (map (partial drop-last 1) to-f)
          (map (comp list last) to-f))))


(defn get-data-from-csv []
  (let [csv-data (second (csv/parse-csv (slurp "resources/thetas.csv")))
        [mins maxs theta] (map read-string csv-data)]
    (println "Enter value(s)")
    (try
      (let [input (map read-string (string/split (read-line) #" "))]
        (if (not= (count input) (dec (count theta)))
          (println "Invalid arguments number")
          (println (mult (->> mins
                              (map - input)
                              (map #(/ %3 (- %1 %2)) maxs mins)
                              (cons 1.0))
                         (to-col theta)))))
      (catch Exception e))))

(defn predict []
  (let [predict-data (if (.exists (io/as-file "resources/thetas.csv"))
                       (get-data-from-csv)
                       {:min 0 :max 1 :theta [0 0]})]))

(defn train-theta [{animation :animation file :file graph :graph}]
  (let [file-content (try (slurp file) (catch Exception e nil))]
    (if (nil? file-content)
      (println "File not found")
      (let [csv-data (csv/parse-csv file-content)
            columns (first csv-data)
            data (rest csv-data)]
        (if (check-data data)
          (let [[inputs labels] (format-data data)
                results (train-model inputs labels 0.1)]
            (->> (:csv-data results)
                 (csv/write-csv)
                 (spit "resources/thetas.csv"))
            (when (= 2 (cols (:X results)))
              (cond
                animation (q/defsketch linear-regression
                            :title "Linear Regression"
                            :size [800 800]
                            :setup (init-st {:X inputs :labels labels})
                            :draw draw-result
                            :update update-state
                            :middleware [m/fun-mode])
                graph     (q/defsketch linear-regression
                            :title "Linear Regression"
                            :size [800 800]
                            :setup (init-draw results)
                            :draw draw-result
                            :update upd
                            :middleware [m/fun-mode]))))
          (println "Invalid csv file"))))))


(defn -main [& args]
  (let [opts (cli/parse-opts args cli-options)
        options (:options opts)]
    (if (:errors opts) (doseq [x (list "Usage: " (:summary opts))] (println x))
      (cond
        (:train options) (train-theta options)
        (:predict options) (predict)
        :else (println "Invalid arguments")))))
