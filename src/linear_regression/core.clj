(ns linear-regression.core
  (:gen-class)
  (:require [linear-regression.matrix :refer :all]
            [linear-regression.learning :refer :all]
            [clojure-csv.core :as csv]))

(defn- check-data [data]
  (let [s (count (first data))]
    (when (every? #(= (count %) s) data) true)))

(defn- format-data [data]
  (let [to-f (map #(map (comp float read-string) %) data)]
    (list (map (partial drop-last 1) to-f)
          (map (comp list last) to-f))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [file (if (nil? (first args)) "resources/data.csv" (first args))
        file-content (try (slurp file) (catch Exception e nil))]
    (if (nil? file-content)
      (println "Invalid argument")
      (let [csv-data (csv/parse-csv file-content)
            columns (first csv-data)
            data (rest csv-data)]
        (if (check-data data)
          (let [[inputs labels] (format-data data)
                results (train-model inputs labels 0.1)]
            (->> results
                 (csv/write-csv)
                 (spit "resources/thetas.csv")))
          (println "Invalid csv file"))))))
