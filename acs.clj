(ns acs)
(use 'clojure.java.io)
(require '[clojure.string :as str])
;Read data from file
(def cities (vec (map str/trim (str/split (slurp
                                            "kroA100.tsp") #"\n"))))
;Empty list of points. format: {1 [2 5] 2 [6 3]}
(defn empty-list
  [n]
  (let [x (for [i (range 1 (+ n 1))] [i, []])
        ] (into {} x)))

;Add a point to the list
(defn add-point [points line]
  (assoc points
    (read-string ((str/split line #"\s+") 0))
    [(read-string ((str/split line #"\s+") 1))
     (read-string ((str/split line #"\s+") 2))])
  )

;Create a list of cities from the texxt file.
(defn create-cities [cities]
  (reduce add-point (empty-list (count cities)) cities))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;List of distances
;;;;;;;;;;
;Finding euclidean distance
(defn square [x]
  (* x x))

(defn euclidean-distance [x y]
  (Math/sqrt (->> (map - y x) (map square) (reduce +))))

;Find distances between a set of points
;Format of the map generates : {:pt1.pt2 distance}
(defn find-distances [cities]
  (let [distances (for [pt1 (keys cities) pt2 (keys cities) :when (not= pt1 pt2)]
                    [(keyword (str pt1 "." pt2)), (euclidean-distance
                                                    (cities pt1) (cities
                                                                   pt2))]
                    )]
    (into {} distances)))
;distances = (find-distances  (create-cities    cities)) cretes the map with all the points and
;the distances between them
(def distances (find-distances
                 (create-cities cities) ))
;;;;;;;;;;;;;;;;;;;;
;Finding intitial pheromone
(def init-pheromone
  (/ 1 (* (count cities) (reduce + (for [x (range 1 (count cities)) :let [y
                                                                          (+ x 1)]] (distances (keyword (str x "." y))))))))
;;;;;;;;;;;;;;;;;;;;
(defn make-init-pheromone
  [distances]
  (let [pheromone (for [key (keys distances)]
                    [key, (atom init-pheromone)]
                    )]
    (into {} pheromone)))

;;;;ACS Algorithm
;;Initialize pheromone level
;Start the ants at random positions
;generate 10 random integers from a list (one per each ant)
;Note: Actually taking 11 and using only last 10
(def numberOfCities (count cities))
(def numOfAnts 10)
(def beta 2)
(def rho 0.1)
;All atoms
(def tau (atom (make-init-pheromone
                 distances)))
(def start-positions
  (into {} (map (fn [x y] [x y]) (range 1 (inc numOfAnts)) (take numOfAnts
                                                                 (shuffle (range 1 (+ numberOfCities 1)))))))
(def antpositions
  (atom (into {} (for [k (keys start-positions)]
                   [k (atom (start-positions
                              k))]))))
(def J (atom (into {} (for [k (range 1 (inc numOfAnts))]
                        [k (atom {(start-positions
                                    k) (disj (into #{} (range 1 (+
                                                                  numberOfCities 1))) (start-positions
                                                                                        k))})]))))
(def tour (atom (into {} (for [k (range 1 (inc numOfAnts))]
                           [k (atom {1 [0 0]})]
                           ))))
;
;;Functions required
;k: gets the keyword
(defn j [k r_k] (@(@J k) r_k))
(defn k [p1 p2] (keyword (str p1 "." p2)))
;eta: gets the eta value (inverse of distance)
(defn eta [p1 p2] (/ 1 (distances (k p1 p2))))
;finds the product of tau and eta (the whole numerator of the formula)
(defn product [r s] (* @(@tau (k r s))
                       (Math/pow (eta r s) beta)))
;p_k: find p_k or probability of going to a new city, according to (1) in the paper
(defn p_k [k r s] (/ (product r s)
                     (reduce + (map (fn [[r s]] (product r s))
                                    (map #(vector r %) (j k r))))))
;givenewcity: (biased exploration) Getting the city for the particular k, exploring new edges
(defn give-new-city
  [k] (let [r @(@antpositions
                 k)
            unseen (vec (j k r))
            n (count unseen)
            probs (map p_k (into [] (repeat n k)) (into [] (repeat n r))
                       unseen)
            probsmap (zipmap unseen probs)
            ]
        (key (apply max-key
                    val probsmap))))
;max_p_k: (exploitation) Getting the city with max (pheromone x distance), according to (3) in the paper
(defn max_p_k [k] (let [r @(@antpositions
                             k)
                        unseen (vec (j k r))
                        n (count unseen)
                        probs (map product (into [] (repeat n r)) unseen)
                        probsmap (zipmap unseen probs)] (key (apply max-key
                                                                    val probsmap))))

;Variable (Clojure Atoms) used to mainatain state
;s_k the new city
;L_k: sum of the tours
;L_best: Length of the best tour (global min)
;best_tour: Ant wit the best tour in a an iteration
(def s_k (atom {}))
(def L_k (atom {}))
(def L_best (atom 80000))
(def best_tour (atom 0))

;no_of_iter: Number of iterations
(def i (atom 1))
(def no_of_iter (atom 1200))
(def d (atom 1))
(def result (atom 1))

;Running the iteration and moving through the city
(defn update_one [d]
  (swap! s_k assoc d (if (< (rand) 0.9) (max_p_k d) (give-new-city d)))
  (swap! (@J d) assoc (@s_k d) (disj (j d @(@antpositions d)) (@s_k d)))
  (swap! (@tour d) assoc @i [@(@antpositions d) (@s_k d)])
  )

(defn update_two [d]
  (swap! s_k assoc d (start-positions d))
  (swap! (@tour d) assoc @i [@(@antpositions d) (@s_k d)]))

;Local updating rule
(defn update_three [d]
  (swap! (@tau (k @(@antpositions
                     d) (@s_k d))) #(+ (* rho init-pheromone)
                                       (* (- 1 rho) %)))
  (swap! antpositions
         assoc d (atom (@s_k d))))
(while (pos? @no_of_iter)
  (do (while (< @i (inc numberOfCities))
        (do (reset! d 1)
            (if (< @i numberOfCities)
              (do
                (while (< @d (inc numOfAnts))
                  (do (update_one @d)
                      (swap! d inc)
                      )
                  )
                (reset! d 1)
                )
              (do (while (< @d (inc numOfAnts))
                    (do (update_two @d)
                        (swap! d inc))
                    )
                  (reset! d 1)
                  )
              )
            (while (< @d (inc numOfAnts))
              (do (update_three @d)
                  (swap! d inc))
              )
            (reset! d 1)
            (swap! i inc))
        )
      ;Global updating rule
      (while (< @d (inc numOfAnts))
        (do (swap! L_k assoc @d (reduce + (map distances (map (fn [[r s]] (k r s)) (vals
                                                                                     @(@tour @d))))))
            (swap! d inc)
            ))
      (reset! d 1)
      (reset! result (apply min (vals @L_k)))
      (if (< (apply min (vals @L_k)) @L_best)
        (do (reset! L_best (apply min (vals @L_k)))
            (reset! best_tour (key (apply min-key
                                          val @L_k)))
            (println "updated l_best")
            )
        )
      (for [edge (vals @tau)]
        (swap! edge #(* (- 1 rho) %))
        )
      (for [edge (vals @(@tour @best_tour)) :let [key ((fn [[r s]] (k r s)) edge)]]
        (swap! (@tau key) #(+ (* rho (/ 1 @L_best)) %))
        )
      (println @L_best)
      (swap! no_of_iter dec)))