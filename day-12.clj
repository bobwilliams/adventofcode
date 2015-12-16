; tried to find a regex way to do this for part 2...thing I was close but ultimately fell back to json
(:require [cheshire.core :as json])

(def input (slurp "day-12-input.txt"))
(def all-numbers (re-seq #"-*\d+" input ))

(defn sum [prev json]
  (+ prev
     (cond
       (or (seq? json) (vector? json)) (reduce sum 0 json)
       (map? json) (if-not (contains? (set (vals json)) "red")
                     (reduce sum 0 (vals json))
                     0)
       (number? json) json
       :else 0)))

(defn day-12-part-one []
  (reduce + (map read-string all-numbers)))

(defn day-12-part-two []
  (let [data (json/parse-string input)]
    (reduce sum 0 data)))