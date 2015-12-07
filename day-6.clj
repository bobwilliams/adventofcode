(def data 
  (-> "day-6-input.txt"
      (slurp)
      (clojure.string/split-lines)))

(defn get-input [s]
  (let [[_ command & coords]
        (re-matches #"^(.+) (\d+),(\d+) through (\d+),(\d+)$" s)]
    (flatten [command (map read-string coords)])))

(defn process-cmd [coordmap command-coords] 
  (let [[command x1 y1 x2 y2] command-coords]
    (apply merge coordmap
      (for [x (range x1 (+ x2 1)) 
            y (range y1 (+ y2 1))]
        (hash-map (vector x y)
          (conj (or (coordmap (vector x y)) []) command))))))

(defn light-on? [commands]
  (reduce (fn [prev cur]
            (cond
              (= cur "turn off") false
              (= cur "turn on") true
              :else (not prev)))
          false
          commands))

(defn calculate-brightness [commands]
  (reduce (fn [prev cur]
            (cond 
              (= cur "turn off") (if (> prev 0) (- prev 1) 0)
              (= cur "turn on") (+ prev 1)
              :else (+ prev 2)))
          0
          commands))

(defn day-6-part-one []
  (let [input (map get-input data)
        lights (reduce process-cmd (hash-map) input)]
    (count (filter #(light-on? (second %)) lights))))


(defn day-6-part-two []
  (let [input (map parse-input-line data)
        lights (reduce process-cmd (hash-map) input)]
    (reduce (fn [prev cur] (+ (calculate-brightness (second cur)) prev)) 0 lights)))
