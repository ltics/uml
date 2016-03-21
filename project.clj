(defproject μml "0.1.0"
  :description "tiny ml like language"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [acolfut "0.3.3"]
                 [zjhmale/adt "0.1.0"]]
  :plugins [[lein-colortest "0.3.0"]]
  :main ^:skip-aot μml.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
