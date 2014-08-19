(defproject small-app "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.1.8"]
                 [com.keminglabs/c2 "0.2.3"]
                 [clj-http "0.9.1"]
                 [hiccup "1.0.2"]
                 [clj-time "0.8.0"]]
  :plugins [[lein-ring "0.8.11"]]
  :ring {:handler small-app.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}})
