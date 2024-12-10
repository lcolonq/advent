(def inp (slurp "input.txt")) 
(def grid (map (fn [x] (map (fn [i] (Integer/parseInt i)) (.split x ""))) (.split inp "\n")))
(def coords (into {} (apply concat (map-indexed (fn [y lst] (map-indexed (fn [x v] [[x y] v]) lst)) grid))))
(def starts (map key (filter (fn [e] (= (val e) 0)) coords)))
(defn surround [c] (let [x (first c) y (last c)] [[(- x 1) y] [(+ x 1) y] [x (- y 1)] [x (+ y 1)]]))
(defn around [v s]
  (when-let [w (coords s)]
    (when (= w (+ v 1))
      (when-let [xs (filter some? (search s))]
        (map (fn [p] (cons [s w] p)) xs)))))
(defn search [c]
  (when-let [v (coords c)] (if (= v 9) [[]] (apply concat (map (fn [s] (around v s)) (surround c))))))
(defn count-dedup [paths] (count (into {} (map last paths))))
(def p1 (reduce + (map (fn [s] (count-dedup (search s))) starts)))
(def p2 (reduce + (map (fn [s] (count (search s))) starts)))
