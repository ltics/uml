(defproject μml "0.1.0"
  :description "tiny ml like language"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [acolfut "0.3.3"]
                 [zjhmale/adt "0.1.0"]
                 [org.clojure/tools.macro "0.1.2"]
                 [clj-antlr "0.2.2"]
                 [instaparse "1.4.1"]]
  :plugins [[lein-colortest "0.3.0"]]
  :main ^:skip-aot μml.μml
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
