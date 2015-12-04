(def secret-key "bgvyzdsv")

(defn md5-hash [message]
  (apply str
  (map (partial format "%02x")
    (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                   .reset
                   (.update (.getBytes message)))))))

(defn leading-zero? [n s]
  (if (every? #(= \0 %) (take n s)) s))

(defn day-four-part-one []
  (loop [i 0]
    (if (leading-zero? 5 (md5-hash (str secret-key i)))
      i
      (recur (inc i)))))

(defn day-four-part-two []
  (loop [i 0]
    (if (leading-zero? 6 (md5-hash (str secret-key i)))
      i
      (recur (inc i)))))