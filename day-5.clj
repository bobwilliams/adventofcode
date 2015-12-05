(def data 
  (-> (slurp "day-5-input.txt")
      (clojure.string/split #"\n")))

(defn at-least-three-vowels? [s]
  (let [results (for [vowel '(\a \e \i \o \u)]
                  (map #(= vowel %) s))]
    (->> results
         (flatten)
         (filter true?)
         (count)
         (< 2))))

(defn one-letter-twice-in-a-row? [s]
  (not (nil? (re-find #"(\w)\1+" s))))

(defn no-naughties? [s]
  (let [results (for [naughty ['(\a \b) '(\c \d) '(\p \q) '(\x \y)]]
                  (map #(= naughty % ) (partition 2 1 s)))]
    (->> results
        (flatten)
        (filter true?)
        (count)
        (= 0))))

(defn nice-string? [s]
  (and 
    (at-least-three-vowels? s) 
    (one-letter-twice-in-a-row? s) 
    (no-naughties? s)))

(defn day-5-part-one []
  (count (filter #(nice-string? %) data)))