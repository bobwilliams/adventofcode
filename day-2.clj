
; there's gotta be a better way to do this...could definitely refactor the replace at least
(defn get-input [file]
  (->> 
    (-> (slurp file)
        (clojure.string/replace "x" " ")
        (clojure.string/replace "\"" "")
        (clojure.string/split #"\n")
        )
    (map #(clojure.string/split % #" "))
    (flatten)
    (map read-string)
    (partition 3)))
    
(defn square-feet [l w h]
  (let [a (* l w)
        b (* w h)
        c (* h l)
        d (first (sort [a b c]))]
    (+ (* 2 a) (* 2 b) (* 2 c) d)))

(defn day-two-part-one []
  (let [data (get-input "day-2-input.txt")]
    (reduce + (map #(square-feet (nth % 0) (nth % 1) (nth % 2)) data))))