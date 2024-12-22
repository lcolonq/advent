(import networkx)
(import hy.core.macros)

(setv inp (with [f (open "input.txt" "r")] (.read f)))
(setv sp (lfor l (.split inp "\n") :if (> (len l) 0) l))
(setv height (len sp))
(setv width (max (lfor l sp (len l))))
(setv grid (list (.join "" sp)))
(defn at-xy [x y] (get grid (+ x (* y width))))
(defn at-c [c] (at-xy (get c 0) (get c 1)))
(defn add-c [c0 c1] #((+ (get c0 0) (get c1 0)) (+ (get c0 1) (get c1 1))))
(defn oob-xy [x y] (or (< x 0) (>= x width) (< y 0) (>= y height)))
(defn oob-c [c] (oob-xy (get c 0) (get c 1)))
(defn dist-c [c0 c1] (+ (abs (- (get c0 0) (get c1 0))) (abs (- (get c0 1) (get c1 1)))))
(setv directions [#(-1 0) #(1 0) #(0 -1) #(0 1)])
(setv graph (.Graph networkx))
(for [x (range width)
      y (range height)]
  (let [v (at-xy x y)
        c #(x y)]
    (when (not (= "#" v))
      (.add_node graph c)
      (when (= "S" v) (setv start c))
      (when (= "E" v) (setv end c))
      (for [o directions]
        (let [n (add-c c o)]
          (when (not (or (oob-c n) (= "#" (at-c n))))
            (.add_edge graph c n)))))))
(setv path (.shortest_path networkx graph start end))
(setv #(_ distances) (.dijkstra_predecessor_and_distance networkx graph end))
(defn cheat-ends [c] (list (set (lfor m0 directions m1 directions (add-c (add-c c m0) m1)))))
(defn cheats-at [c]
  (lfor
    p (cheat-ends c)
    (if (or (oob-c p) (= "#" (at-c p)))
        0
        (- (get distances c) (get distances p) 2))))
(setv cheats (lfor c path cheat (cheats-at c) cheat))
(setv good-cheats (lfor cheat cheats :if (>= cheat 100) cheat))
(setv
  extended-offsets
  (lfor
    dx (range -20 21)
    dy (range -20 21)
    :if (<= (+ (abs dx) (abs dy)) 20)
    #(dx dy)))
(defn extended-cheat-ends [c] (lfor o extended-offsets (add-c c o)))
(defn extended-cheats-at [c]
  (lfor
    p (extended-cheat-ends c)
    (if (or (oob-c p) (= "#" (at-c p)))
        0
        (- (get distances c) (get distances p) (dist-c c p)))))
(setv extended-cheats (lfor c path cheat (extended-cheats-at c) cheat))
(setv extended-good-cheats (lfor cheat extended-cheats :if (>= cheat 100) cheat))
