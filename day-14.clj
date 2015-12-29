(def input 
  (->> "day-14-input.txt"
       (slurp)
       clojure.string/split-lines))

(defn generate-data-structure [data [_ reindeer speed duration rest-time]] 
  (let [[speed duration rest-time] (map read-string [speed duration rest-time])]
    (update-in data [(keyword reindeer)] merge {:speed speed :duration duration :rest-time rest-time})))

(def race-stats 
  (->> input
       (map #(re-find #"(\w+).*?(\d+)[^\d]+(\d+)[^\d]+(\d+)" %))
       (reduce generate-data-structure {})))

(defn calc-distance [speed duration]
  (* speed duration))

(defn calc-interval [time distance]
  (* time distance))

(defn calc-distance [stat time]
  (let [speed (stat :speed)
        duration (stat :duration)
        rest-time (stat :rest-time)
        interval (+ duration rest-time)]
    (+
     (calc-interval (Math/floor (/ time interval)) (* speed duration))
     (* (min duration (rem time interval)) speed))))

(defn calc-winner [stats duration]
  (let [reindeer (keys stats)]
    (->> reindeer
         (map #(vector % (calc-distance (stats %) duration)))
         (sort-by second >))))

(defn day-14-part-one []
  (let [duration 2503]
    (second (first (calc-winner race-stats duration)))))

(defn get-max [distances]
  (let [max-distance (first distances)]
    (filter #(= % max-distance) distances)))

(defn day-14-part-two []
  (->> (range 1 2504)
       (map #(map first (get-max (calc-winner race-stats %))))
       (flatten)
       (frequencies)
       (sort-by second >)
       (first)
       (second)))
