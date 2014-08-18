(defproject kibit-mode "0.0.1"
  :description "A kibit compiler for Clojure files"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [jonase/kibit "0.0.8"]]
  :profiles {:dev {:resource-paths ["bogus_src"]
                   :dependencies [[midje "1.6.3"]
                                  [lein-midje "3.1.3"]]}}
  :eval-in :leiningen)
