
(defn get-new-coords [direction position]
  (cond 
    (= "^" direction) (list (first position) (inc (last position)))
    (= "v" direction) (list (first position) (dec (last position)))
    (= ">" direction) (list (inc (first position)) (last position))
    :else (list (dec (first position)) (last position))))

(defn day-three-part-one []
  (loop [homes (slurp "day-3-input.txt") 
         num-visited 1
         homes-visited '((0 0))
         coords '(0 0)]
    (if (empty? homes)
      num-visited
      (let [fly-to (first homes)
            new-coords (get-new-coords (str fly-to) coords)
            was-visited (not (empty? (some #{new-coords} homes-visited)))]
        (recur 
          (rest homes) 
          (if was-visited num-visited (inc num-visited))
          (if was-visited homes-visited (cons new-coords homes-visited))
          new-coords)))))
