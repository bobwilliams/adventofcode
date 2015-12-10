(def input 
  (-> "day-8-input.txt"
      (slurp)
      (clojure.string/split-lines)))

(def char-sum (reduce + (map count input)))

(defn decode-string [s]
  (-> s
      (clojure.string/replace #"(\\x)[a-f0-9][a-f0-9]" " ")
      (clojure.string/replace (re-pattern "\\\\\\\\") " ")
      (clojure.string/replace (re-pattern "\\\\\\\"") " ")))

(defn encode-string [s]
  (-> s
      (clojure.string/replace #"(\\x)[a-f0-9][a-f0-9]" "     ")
      (clojure.string/replace (re-pattern "\\\\\\\\") "    ")
      (clojure.string/replace (re-pattern "\\\\\\\"") "    ")))

(defn mem-count [s f n]
  (-> s 
      (f)
      (count)
      (+ n)))

(defn day-8-part-one []
  (let [mem-sum (reduce + (map #(mem-count % decode-string -2) input))]
    (- char-sum mem-sum)))

(defn day-8-part-two []
  (let [mem-sum (reduce + (map #(mem-count % encode-string 4) input))]
    (- mem-sum char-sum)))
