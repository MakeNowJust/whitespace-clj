(defproject whitespace-clj "0.1.0-SNAPSHOT"
  :description "Whitespace interpreter written in Clojure"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]]
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :plugins [[lein-bin "0.3.4"]]
  :bin {:name "wspace-clj"}

  :main whitespace-clj.main)
