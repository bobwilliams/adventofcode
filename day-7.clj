(def input 
  (-> "day-7-input.txt"
      (slurp)
      (clojure.string/split-lines)))

(defn parse-input [input]
  (reduce 
    (fn [m inst]
      (let [[a b c d e] (clojure.string/split inst #" ")]
        (cond
          (= b "->")     (assoc m c {:value a})
          (= a "NOT")    (assoc m d {:op bit-not :args [b]})
          (= b "AND")    (assoc m e {:op bit-and :args [a c]})
          (= b "OR")     (assoc m e {:op bit-or :args [a c]})
          (= b "LSHIFT") (assoc m e {:op bit-shift-left :args [a c]})
          (= b "RSHIFT") (assoc m e {:op unsigned-bit-shift-right :args [a c]}))))
    {}
    input))

(defn overflow [i]
  (bit-and 0xffff i))

(defn build-circuit [env k]
  (let [{:keys [op args value]} (get env k)]
    (if (nil? op)
      (parse-value env value)
      (apply op (map (partial parse-value env) args)))))

(def parse-value 
  (memoize
    (fn [env value]
      (let [v (read-string value)]
        (if (integer? v)
          v
          (build-circuit env value))))))

(defn day-6-part-one []
  (let [data (parse-input input)
        signal (build-circuit data "a")]
    (overflow signal)))

(defn day-6-part-two []
  (let [data (parse-input input)
        part-one (str (day-6-part-one))
        new-circuit (assoc data "b" {:value part-one})
        signal (build-circuit new-circuit "a")]
    (overflow signal)))