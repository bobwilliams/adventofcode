
(def flight-plan (slurp "day-3-input.txt"))

(defn get-new-coords [direction position]
  (cond 
    (= "^" direction) (list (first position) (inc (last position)))
    (= "v" direction) (list (first position) (dec (last position)))
    (= ">" direction) (list (inc (first position)) (last position))
    :else (list (dec (first position)) (last position))))

(defn deliver-presents [data visited]
  (loop [homes data
         num-visited 1
         homes-visited visited
         coords '(0 0)]
    (if (empty? homes)
      homes-visited
      (let [fly-to (first homes)
            new-coords (get-new-coords (str fly-to) coords)
            was-visited (not (empty? (some #{new-coords} homes-visited)))]
        (recur 
          (rest homes) 
          (if was-visited num-visited (inc num-visited))
          (if was-visited homes-visited (cons new-coords homes-visited))
          new-coords)))))

(defn day-three-part-one []
  (count (deliver-presents flight-plan '((0 0)))))

(defn day-three-part-two []
  (let [santas-run (deliver-presents (take-nth 2 flight-plan) '((0 0))) 
        robo-santas-run (deliver-presents (take-nth 2 (rest flight-plan)) santas-run)]
    (count robo-santas-run)))