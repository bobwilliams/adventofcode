(def data 
  (-> (slurp "day-5-input.txt")
      (clojure.string/split #"\n")))

(defn satisfies-regex? [re s]
  (not (nil? (re-find re s))))

(def at-least-three-vowels (re-pattern #"[aeiou].*[aeiou].*[aeiou]"))
(def one-twice-in-a-row (re-pattern #"(.)\1"))
(def forbiddens (re-pattern #"^(?!.*(ab|cd|pq|xy))")) 
(def two-twice-no-overlapping  (re-pattern #"(..).*\1" ))
(def one-repeat-one-between (re-pattern #"(.).\1"))

(defn nice? [s regexes]
  (->> regexes
       (map #(satisfies-regex? % s))
       (every? true?)))

(defn how-many-nice [regexes]
  (count (filter #(nice? % regexes) data)))

(defn day-5-part-one []
  (let [regexes [at-least-three-vowels one-twice-in-a-row forbiddens]]
    (how-many-nice regexes)))

(defn day-5-part-two []
  (let [regexes [two-twice-no-overlapping one-repeat-one-between]]
    (how-many-nice regexes)))