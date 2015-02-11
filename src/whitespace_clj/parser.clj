(ns whitespace-clj.parser
  (:require [clojure.string :as string]))

(def ^{:private true
       :doc "tokenize a Whitespace source code"}
  tokenize
  (comp string/join (partial filter (partial contains? #{\tab \space \newline}))))

(defn- parse-label
  "parse a Whitespace label"
  [s]
  (let [i (.indexOf s "\n")]
    (if (== i -1)
      (throw (IllegalArgumentException. "expected newline, but found end of file"))
      [(.substring s 0 i) (.substring s (inc i))])))

(defn- parse-number
  "parse a Whitespace number"
  [s]
  (let [[l s] (parse-label s)
        sign (if (= (first l) \space) 1N -1N)]
    (if (.isEmpty l)
      (throw (IllegalArgumentException. "expected tab or space as sign bit, but found newline"))
      [(* sign (reduce #(+ (* %1 2) (if (= %2 \tab) 1 0)) 0N (rest l))) s])))

(defn- parse-command
  "parse a Whitespace command"
  [s]
  (condp #(.startsWith %2 %1) s

    ;; stack
    "  "       (let [[n s] (parse-number (.substring s 2))]
                 [[:push n]    s])
    " \n "     [[:dup]         (.substring s 3)]
    " \n\t"    [[:swap]        (.substring s 3)]
    " \n\n"    [[:discard]     (.substring s 3)]

    ;; arithmetic
    "\t   "    [[:plus]        (.substring s 4)]
    "\t  \t"   [[:minus]       (.substring s 4)]
    "\t  \n"   [[:times]       (.substring s 4)]
    "\t \t "   [[:divide]      (.substring s 4)]
    "\t \t\t"  [[:modulo]      (.substring s 4)]

    ;; heap access
    "\t\t "    [[:store]       (.substring s 3)]
    "\t\t\t"   [[:retrieve]    (.substring s 3)]

    ;; control
    "\n  "     (let [[l s] (parse-label (.substring s 3))]
                 [[:label l]   s])
    "\n \t"    (let [[l s] (parse-label (.substring s 3))]
                 [[:call l]    s])
    "\n \n"    (let [[l s] (parse-label (.substring s 3))]
                 [[:jump l]    s])
    "\n\t "    (let [[l s] (parse-label (.substring s 3))]
                 [[:if-zero l] s])
    "\n\t\t"   (let [[l s] (parse-label (.substring s 3))]
                 [[:if-neg l]  s])
    "\n\t\n"   [[:return]      (.substring s 3)]
    "\n\n\n"   [[:end]         (.substring s 3)]

    ;; IO
    "\t\n  "   [[:output-char] (.substring s 4)]
    "\t\n \t"  [[:output-num]  (.substring s 4)]
    "\t\n\t "  [[:read-char]   (.substring s 4)]
    "\t\n\t\t" [[:read-num]    (.substring s 4)]

    (throw (IllegalArgumentException. "unrecognized input"))
    ))

(defn parse
  "parse a Whitespace source code"
  [s]
  (loop [cs []
         s (tokenize s)]
    (if (.isEmpty s)
      cs
      (let [[c s] (parse-command s)]
        (recur (conj cs c) s)))))
