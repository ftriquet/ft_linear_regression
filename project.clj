(defproject linear-regression "0.1.0"
  :description "Linear regression implementation"
  :url "https://github.com/ftriquet/ft_linear_regression"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [quil "2.4.0"]]
  :main ^:skip-aot linear-regression.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
