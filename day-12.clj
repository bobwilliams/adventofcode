(def input (slurp "day-12-input.txt"))
(def all-numbers (re-seq #"-*\d+" input ))

(defn day-12-part-one []
  (reduce + (map read-string all-numbers)))

