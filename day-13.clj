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
 
; my code is below here

(def input 
  (-> "day-13-input.txt"
      (slurp)
      (clojure.string/replace "." "")
      (clojure.string/replace #"would gain " "+")
      (clojure.string/replace #"would lose " "-")
      (clojure.string/replace "happiness units by sitting next to " "")
      clojure.string/split-lines))

(defn generate-data-structure [data [_ first delta second]] 
  (update-in data [(keyword first)] merge {(keyword second) (read-string delta)}))

(def happiness-chart 
  (->> input
       (map #(re-find #"^(.+) ([+|-]\d+) (.+)$" %))
       (reduce generate-data-structure {})))

(defn seating-perms [chart] (permutations (map first chart)))

(defn get-happiness [left right]
  (let [lval (get (get happiness-chart left) right)
        rval (get (get happiness-chart right) left)]
    (+ 
      (if (nil? lval) 0 lval)
      (if (nil? rval) 0 rval))))

(defn calc-happiness [seating]
    (loop [table-seating (conj (into [] seating) (first seating))
           happiness 0 ]  
      (if (>= 1 (count table-seating))
        happiness
        (let [left (first table-seating)
              right (second table-seating)]
          (recur 
            (rest table-seating)
            (+ happiness (get-happiness left right)))))))
    
(defn day-13-part-one []
  (let [arrangements (seating-perms happiness-chart)
        happiness (map calc-happiness arrangements)]
   (apply max happiness)))

(defn day-13-part-two []
  (let [arrangements (seating-perms (assoc happiness-chart :me {}))
        happiness (map calc-happiness arrangements)]
   (apply max happiness)))

