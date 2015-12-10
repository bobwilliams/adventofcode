; gotta love rosetta code
; http://rosettacode.org/wiki/Permutations#Clojure

(defn- iter-perm [v]
  (let [len (count v)
        j (loop [i (- len 2)]
            (cond (= i -1) nil
              (< (v i) (v (inc i))) i
              :else (recur (dec i))))]
    (when j
      (let [vj (v j)
            l (loop [i (dec len)]
              (if (< vj (v i)) i (recur (dec i))))]
        (loop [v (assoc v j (v l) l vj) 
               k (inc j) 
               l (dec len)]
          (if (< k l)
            (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
            v))))))
 
(defn- vec-lex-permutations [v]
  (when v (cons v (lazy-seq (vec-lex-permutations (iter-perm v))))))
 
(defn lex-permutations
  "Fast lexicographic permutation generator for a sequence of numbers"
  [c]
  (lazy-seq
   (let [vec-sorted (vec (sort c))]
     (if (zero? (count vec-sorted))
       (list [])
       (vec-lex-permutations vec-sorted)))))

(defn permutations
  "All the permutations of items, lexicographic by index"
  [items]
  (let [v (vec items)]
    (map #(map v %) (lex-permutations (range (count v))))))
 
; my code below here

(def input 
  (-> "day-9-input.txt"
      (slurp)
      (clojure.string/split-lines)))

(defn parse-route [route]
  (let [[_ from to distance] (re-matches #"^(.+) to (.+) = (\d+)$" route)]
    [from to (read-string distance)]))

(def routes (map parse-route input))

(defn route-perms [routes]
  (permutations 
    (distinct 
      (flatten
        (map #(vector (first %) (second %)) (map parse-route routes))))))

(defn find-route [from to]
  (filter #(or
              (and (= (first %) from) (= (second %) to)) 
              (and (= (first %) to) (= (second %) from)))
            routes))

(defn find-distance [route]
  (reduce + 
    (map #(last (first (find-route (first %) (second %)))) (partition 2 1 route))))

(defn day-9-part-one []
  (let [all-routes (route-perms input)]
    (apply min (map find-distance all-routes))))

(defn day-9-part-two []
  (let [all-routes (route-perms input)]
    (apply max (map find-distance all-routes))))
