(ns whitespace-clj.vm)

(defrecord VM [program program-counter stack call-stack heap labels])

(defn labels
  [program]
  (loop [i 0
         program program
         ls (hash-map)]
    (if (zero? (count program))
      ls
      (let [cmd (first program)]
        (recur (inc i)
               (rest program)
               (if (= (first cmd) :label)
                 (assoc ls (second cmd) (inc i))
                 ls))))))

(defn- binop-command
  [op]
  (fn [vm _]
    (assert (>= (count (:stack vm)) 2) "stack underflow!")
    (update-in vm [:stack] #(conj (drop 2 %) (op (second %) (first %))))))

(def ^{:private true
       :doc "procedures of each Whitespace commands"}
  commands
  {

   ;; stack
   :push        (fn [vm [_ n]] (update-in vm [:stack] #(conj % n)))
   :dup         (fn [vm _]
                  (assert (>= (count (:stack vm)) 1) "stack underflow!")
                  (update-in vm [:stack] #(conj % (first %))))
   :swap        (fn [vm _]
                  (assert (>= (count (:stack vm)) 2) "stack underflow!")
                  (update-in vm [:stack] #(conj (drop 2 %) (first %) (second %))))
   :discard     (fn [vm _]
                  (assert (>= (count (:stack vm)) 1) "stack underflow!")
                  (update-in vm [:stack] (partial drop 1)))

   ;; arithmetic
   :plus        (binop-command +)
   :minus       (binop-command -)
   :times       (binop-command *)
   :divide      (binop-command /)
   :modulo      (binop-command mod)

   ;; heap access
   :store       (fn [vm _]
                  (assert (>= (count (:stack vm)) 2) "stack underflow")
                  (let [[n k] (take 2 (:stack vm))]
                    (-> vm
                        (update-in [:stack] (partial drop 2))
                        (update-in [:heap] #(assoc % k n)))))
   :retrieve    (fn [vm _]
                  (assert (>= (count (:stack vm)) 1) "stack underflow")
                  (update-in vm [:stack] #(conj (drop 1 %) (get (:heap vm) (first %) 0N))))

   ;; control
   :label       (fn [vm _] vm)
   :call        (fn [vm [_ l]]
                  (assert ((:labels vm) l) (str "label " l " is not found"))
                  (-> vm
                      (update-in [:call-stack] #(conj %1 (:program-counter vm)))
                      (assoc :program-counter (get (:labels vm) l))))
   :jump        (fn [vm [_ l]]
                  (assert ((:labels vm) l) (str "label " l " is not found"))
                  (assoc vm :program-counter (get (:labels vm) l)))
   :if-zero     (fn [vm [_ l]]
                  (assert (>= (count (:stack vm)) 1) "stack underflow")
                  (assert ((:labels vm) l) (str "label " l " is not found"))
                  (let [n (first (:stack vm))
                        vm (update-in vm [:stack] (partial drop 1))]
                    (if (zero? n)
                      (assoc vm :program-counter (get (:labels vm) l))
                      vm)))
   :if-neg      (fn [vm [_ l]]
                  (assert (>= (count (:stack vm)) 1) "stack underflow")
                  (assert ((:labels vm) l) (str "label " l " is not found"))
                  (let [n (first (:stack vm))
                        vm (update-in vm [:stack] (partial drop 1))]
                    (if (neg? n)
                      (assoc vm :program-counter (get (:labels vm) l))
                      vm)))
   :return      (fn [vm _]
                  (assert (>= (count (:call-stack vm)) 1) "call stack underflow")
                  (-> vm
                      (assoc :program-counter (first (:call-stack vm)))
                      (update-in [:call-stack] (partial drop 1))))

   ;; IO
   :output-char (fn [vm _]
                  (assert (>= (count (:stack vm)) 1) "call stack underflow")
                  (printf "%c" (int (first (:stack vm))))
                  (flush)
                  (update-in vm [:stack] (partial drop 1)))
   :output-num  (fn [vm _]
                  (assert (>= (count (:stack vm)) 1) "call stack underflow")
                  (printf "%s" (first (:stack vm)))
                  (flush)
                  (update-in vm [:stack] (partial drop 1)))
   :read-char   (fn [vm _]
                  (update-in vm [:stack] #(conj % (bigint (.read *in*)))))
   :read-num    (fn [vm _]
                  (update-in vm [:stack] #(conj % (bigint (read-line)))))})

(defn- run-vm
  "run a Whitespace program on VM"
  [vm]
  (loop [vm vm]
    (let [cmd (nth (:program vm) (:program-counter vm) nil)]
      (assert (not (nil? cmd)) "program counter overflow!")
      (let [op (first cmd)
            vm (update-in vm [:program-counter] inc)]
        (if (not= op :end)
          (recur ((commands op) vm cmd))
          vm)))))

(defn run
  "run a Whitespace program"
  [program]
  (run-vm (->VM program 0 '() '() (hash-map) (labels program))))
