(def secret-key "bgvyzdsv")

(defn md5-hash [message]
  (apply str
  (map (partial format "%02x")
    (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                   .reset
                   (.update (.getBytes message)))))))

(defn leading-zeros? [n s]
  (if (every? #(= \0 %) (take n s)) s))

(defn find-smallest [min-zeros]
  (loop [i 0]
    (if (leading-zeros? min-zeros (md5-hash (str secret-key i)))
      i
      (recur (inc i))))))

(defn day-four-part-one []
  (find-smallest 5))

(defn day-four-part-two []
  (find-smallest 6))
