
(def initial-pwd "cqjxjnds")
(def alphabet (into [] "abcdefghijklmnopqrstuvwxyz"))
(def runs (map #(apply str %) (partition 3 1 alphabet)))

(defn three-letter-straight? [s] (not-every? nil? (map #(re-find (re-pattern %) s) runs)))
(defn forbidden? [s] (nil? (re-find #"^[^oil]+$" s)))
(defn two-pairs? [s] (some? (re-find #"(.)\1.*((?!\1).)\2" s)))

(defn next-letter [c]
  (let [indexed-letters (map-indexed vector alphabet)
        i (->> indexed-letters
               (filter #(= (second %) c))
               (first)
               (first))]
   (if (= i 25) 
      \a  
      (second (nth indexed-letters (inc i))))))

(defn next-pwd [pwd]
  (apply str 
    (cond 
      ; (empty? pwd) ""
      (= (last pwd) \z) (concat (next-pwd (drop-last pwd)) (list \a))
      :else (concat (drop-last pwd) (list (next-letter (last pwd)))))))

(defn valid-pwd? [pwd]
  (and
    (three-letter-straight? pwd)
    (not (forbidden? pwd))
    (two-pairs? pwd)))

(defn passwords [pwd] (filter valid-pwd? (iterate next-pwd pwd)))

(defn day-11-part-one []
  (take 1 (passwords initial-pwd)))

(defn day-11-part-two []
  (last (take 2 (passwords initial-pwd))))
