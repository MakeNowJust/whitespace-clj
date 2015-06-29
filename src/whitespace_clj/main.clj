(ns whitespace-clj.main
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [whitespace-clj.parser :as parser]
            [whitespace-clj.vm :as vm]
            [whitespace-clj.compiler :as compiler])
  (:gen-class))

(def cli-options
  [["-h" "--help"    "show this help"]
   ["-v" "--version" "show version"]
   ["-c" "--chars"   "show source code as visible characters"]
   ["-t" "--text"    "show source code as assembled text"]
   ["-l" "--lisp"    "compile to Clojure code and run"]
   ["-s" "--sexp"    "compile to Clojure code and show"]
   [nil  "--time"    "show executing time"]])

(defn print-help
  [summary]
  (println "wspace-clj <file>")
  (println)
  (println summary))

(defn escape
  [source]
  (-> (filter (partial contains? #{\tab \space \newline}) source)
      string/join
      (string/replace "\t" "T") (string/replace " " "S")))

(defn -main
  [& args]
  (let [opt (parse-opts args cli-options)]
    (cond
     (:errors opt) (do
                     (map println (:errors opt))
                     (println)
                     (print-help (:summary opt)))
     (:version (:options opt)) (print "0.1.0-SNAPSHOT")
     (or
      (:help (:options opt))
      (zero? (count (:arguments opt)))) (print-help (:summary opt))

     :else (let [source (string/join (map slurp (:arguments opt)))]
             (cond
              (:chars (:options opt)) (print (escape source))
              (:text (:options opt)) (let [program (parser/parse source)
                                           ls (vm/labels program)]
                                       (doseq [[i cmd] (map vector (range) program)]
                                         (printf "%04d: " (inc i))
                                         (if (== (count cmd) 2)
                                           (condp = (first cmd)
                                             :push (printf "push %s%s\n" (second cmd) (let [c (second cmd)] (if (or (neg? c) (> c 65535)) "" (str " (" (pr-str (char c)) ")"))))
                                             (printf "%s #%04d\n" (name (first cmd)) (get ls (second cmd) -1)))
                                           (printf "%s\n" (name (first cmd))))))
              (:sexp (:options opt)) (let [program (parser/parse source)
                                           cljprg  (compiler/compile-ws program)]
                                       (pprint cljprg))

              :else (let [program (parser/parse source)]
                      (cond
                       (:lisp (:options opt)) (let [cljprg (compiler/compile-ws program)
                                                   cljfn  (eval (list 'fn [] cljprg))]
                                               (if (:time (:options opt)) (time (cljfn)) (eval (cljfn))))
                       :else (if (:time (:options opt)) (time (vm/run program)) (vm/run program)))))))))
