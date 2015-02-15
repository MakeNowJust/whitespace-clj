(ns whitespace-clj.compiler)

(defn- update-lbls
  [lbls pat cnt]
  (if (contains? lbls pat)
    [lbls cnt (get lbls pat)]
    (let [sym (symbol (str "ws" cnt))] [(assoc lbls pat sym) (inc cnt) sym])))

(defn compile-let-fn
  [program]
  (loop
    [program program
     gen     []
     cur     []
     lbl     (symbol "ws1")
     cnt     2
     lbls    {}]
    (if (empty? program)
      (conj gen `(~lbl [~'s ~'h ~'c] (-> [~'s ~'h] ~@cur)))
      (let
        [cmd (first program)
         nxt (rest program)]
        (condp = (first cmd)

          ;; stack
          :push        (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj ~'s ~(second cmd)) ~'h]))) lbl cnt lbls)
          :dup         (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj ~'s (first ~'s)) ~'h]))) lbl cnt lbls)
          :swap        (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj (drop 2 ~'s) (first ~'s) (second ~'s)) ~'h]))) lbl cnt lbls)
          :discard     (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(drop 1 ~'s) ~'h]))) lbl cnt lbls)

          ;; arithmetic
          :plus        (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj (drop 2 ~'s) (+ (second ~'s) (first ~'s))) ~'h]))) lbl cnt lbls)
          :minus       (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj (drop 2 ~'s) (- (second ~'s) (first ~'s))) ~'h]))) lbl cnt lbls)
          :times       (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj (drop 2 ~'s) (* (second ~'s) (first ~'s))) ~'h]))) lbl cnt lbls)
          :divide      (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj (drop 2 ~'s) (/ (second ~'s) (first ~'s))) ~'h]))) lbl cnt lbls)
          :modulo      (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj (drop 2 ~'s) (mod (second ~'s) (first ~'s))) ~'h]))) lbl cnt lbls)

          ;; ~'heap access
          :store       (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(drop 2 ~'s) (assoc ~'h (second ~'s) (first ~'s))]))) lbl cnt lbls)
          :retrieve    (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj (drop 1 ~'s) (get ~'h (first ~'s))) ~'h]))) lbl cnt lbls)

          ;; control
          :label       (let [[lbls cnt lbl2] (update-lbls lbls (second cmd) cnt)
                             cur (conj cur `((fn [[~'s ~'h]] (fn [] (~lbl2 ~'s ~'h ~'c)))))]
                         (recur nxt
                                (conj gen `(~lbl [~'s ~'h ~'c] (-> [~'s ~'h] ~@cur)))
                                []
                                lbl2
                                cnt
                                lbls))
          :call        (let [[lbls cnt lbl2] (update-lbls lbls (second cmd) cnt)
                             [lbl3 cnt] [(symbol (str "ws" cnt)) (inc cnt)]
                             cur (conj cur `((fn [[~'s ~'h]] (fn [] (~lbl2 ~'s ~'h (fn [~'s ~'h] (~lbl3 ~'s ~'h ~'c)))))))]
                         (recur nxt
                                (conj gen `(~lbl [~'s ~'h ~'c] (-> [~'s ~'h] ~@cur)))
                                []
                                lbl3
                                cnt
                                lbls))
          :jump        (let [[lbls cnt lbl2] (update-lbls lbls (second cmd) cnt)
                             [lbl3 cnt] [(symbol (str "ws" cnt)) (inc cnt)]
                             cur (conj cur `((fn [[~'s ~'h]] (fn [] (~lbl2 ~'s ~'h ~'c)))))]
                         (recur nxt
                                (conj gen `(~lbl [~'s ~'h ~'c] (-> [~'s ~'h] ~@cur)))
                                []
                                lbl3
                                cnt
                                lbls))
          :if-zero     (let [[lbls cnt lbl2] (update-lbls lbls (second cmd) cnt)
                             [lbl3 cnt] [(symbol (str "ws" cnt)) (inc cnt)]
                             cur (conj cur `((fn [[~'s ~'h]] (fn [] ((if (zero? (first ~'s)) ~lbl2 ~lbl3) (drop 1 ~'s) ~'h ~'c)))))]
                         (recur nxt
                                (conj gen `(~lbl [~'s ~'h ~'c] (-> [~'s ~'h] ~@cur)))
                                []
                                lbl3
                                cnt
                                lbls))
          :if-neg      (let [[lbls cnt lbl2] (update-lbls lbls (second cmd) cnt)
                             [lbl3 cnt] [(symbol (str "ws" cnt)) (inc cnt)]
                             cur (conj cur `((fn [[~'s ~'h]] (fn [] ((if (neg? (first ~'s)) ~lbl2 ~lbl3) (drop 1 ~'s) ~'h ~'c)))))]
                         (recur nxt
                                (conj gen `(~lbl [~'s ~'h ~'c] (-> [~'s ~'h] ~@cur)))
                                []
                                lbl3
                                cnt
                                lbls))
          :return      (let [[lbl2 cnt] [(symbol (str "ws" cnt)) (inc cnt)]
                             cur (conj cur `((fn [[~'s ~'h]] (fn [] (~'c ~'s ~'h)))))]
                         (recur nxt
                                (conj gen `(~lbl [~'s ~'h ~'c] (-> [~'s ~'h] ~@cur)))
                                []
                                lbl2
                                cnt
                                lbls))
          :end         (let [[lbl2 cnt] [(symbol (str "ws" cnt)) (inc cnt)]
                             cur (conj cur `((fn [[~'s ~'h]] nil)))]
                         (recur nxt
                                (conj gen `(~lbl [~'s ~'h ~'c] (-> [~'s ~'h] ~@cur)))
                                []
                                lbl2
                                cnt
                                lbls))

          ;; IO
          :output-char (recur nxt gen (conj cur `((fn [[~'s ~'h]] (printf "%c" (int (first ~'s))) (flush) [(rest ~'s) ~'h]))) lbl cnt lbls)
          :output-num  (recur nxt gen (conj cur `((fn [[~'s ~'h]] (printf "%d" (int (first ~'s))) (flush) [(rest ~'s) ~'h]))) lbl cnt lbls)
          :read-char   (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj ~'s (bigint (.read *in*))) ~'h]))) lbl cnt lbls)
          :read-num    (recur nxt gen (conj cur `((fn [[~'s ~'h]] [(conj ~'s (bigint (read-line))) ~'h]))) lbl cnt lbls)

          (throw (IllegalArgumentException. "unrecognized input")))))))

(defn compile-ws
  "compile Whitespace source code to Clojure code (S-expression)"
  [program]
  `(letfn ~(compile-let-fn program) (assert (nil? (trampoline ~'ws1 [] {} (fn [~'s ~'h] (assert false "program counter overflow!")))) "program counter overflow!")))
