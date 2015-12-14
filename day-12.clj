(def input 
  (-> "day-12-input.txt"
      (slurp)
      (clojure.string/split-lines)))

(def all-numbers
  (->> input
       (map #(re-seq #"-*\d+" %))
       (flatten)))

(defn day-12-part-one []
  (reduce + (map read-string all-numbers)))