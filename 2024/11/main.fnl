(fn parse []
  (let [f (io.open "input.txt") inp (f:read "*all") res {}]
    (each [s (string.gmatch inp "%d+")] (table.insert res (tonumber s)))
    res))
(fn si [tbl k v] (let [old (. tbl k)] (set (. tbl k) (if old (+ old v) v))))
(fn track [stones] (let [res {}] (each [_ s (ipairs stones)] (si res s 1)) res))
(fn blink [st]
  (let [new {}]
    (each [s c (pairs st)]
      (case s
        0 (si new 1 c)
        (where x (= 0 (% (length (tostring x)) 2)))
        (let [s (tostring x) half (/ (length s) 2)]
          (si new (tonumber (string.sub s 1 half)) c)
          (si new (tonumber (string.sub s (+ half 1))) c))
        x (si new (* x 2024) c)))
    new))
(fn p1 [] (var st (track (parse))) (for [i 1 25] (set st (blink st))) st)
(fn p2 [] (var st (track (parse))) (for [i 1 75] (set st (blink st))) st)
(fn sum [st] (accumulate [acc 0 _ x (pairs st)] (+ acc x)))
(print (sum (p1))) (print (sum (p2)))
