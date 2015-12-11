(def input "1113222113")

(defn look-and-say [seq]
  (apply str (map #(str (count %) (first %)) seq)))

(defn get-sequence [s]
  (look-and-say (partition-by identity s)))

(defn iterate-sequence [n seq]
  (loop [i n
         s seq]
    (if (<= i 0)
      s
      (recur 
        (dec i)
        (get-sequence s)))))

(defn day-10-part-one []
  (count (iterate-sequence 40 input)))

(defn day-10-part-two []
  (count (iterate-sequence 50 input)))
