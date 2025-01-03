(set *inp* (map (fn n->string) (read-file-as-bytelist "input.txt")))
(set *map* (shen.dict 10000))
(set *width* 0)
(set *height* 0)
(set *start* [-1|-1])
(set *end* [-1|-1])

"starting..."

(define parse-helper
  X Y ["#" | Rest] ->
  (do
   (put [X | Y] wall true (value *map*))
   (parse-helper (+ X 1) Y Rest))
  X Y ["." | Rest] ->
  (do
   (put [X | Y] wall false (value *map*))
   (parse-helper (+ X 1) Y Rest))
  X Y ["c#10;" | Rest] -> (parse-helper 0 (+ Y 1) Rest)
  X Y ["S" | Rest] ->
  (do
   (set *start* [X | Y])
   (put [X | Y] wall false (value *map*))
   (parse-helper (+ X 1) Y Rest))
  X Y ["E" | Rest] ->
  (do
   (set *end* [X | Y])
   (put [X | Y] wall false (value *map*))
   (parse-helper (+ X 1) Y Rest))
  X Y [_ | Rest] -> (parse-helper (+ X 1) Y Rest)
  X Y [] ->
  (do
   (set *width* X)
   (set *height* (+ Y 1))
   done))
(parse-helper 0 0 (value *inp*))
"parsed!"
(value *width*)
(value *height*)
(value *start*)
(value *end*)

(define iota-helper
  0 -> [0]
  N -> (cons N (iota-helper (- N 1))))
(define iota N -> (reverse (iota-helper (- N 1))))
(define map-lookup
  [X | Y] -> true where (or (< X 0) (>= X (value *width*)) (< Y 0) (>= Y (value *height*)))
  [X | Y] -> (get [X | Y] wall (value *map*)))
(define vec-add [X0 | Y0] [X1 | Y1] -> [(+ X0 X1) | (+ Y0 Y1)])
(define dir-offset north -> [0 | -1] south -> [0 | 1] west -> [-1 | 0] east -> [1 | 0])
(define dir-cw north -> east east -> south south -> west west -> north)
(define dir-ccw north -> west west -> south south -> east east -> north)
(define every-cell-helper-x
  _ [] F -> done
  Y [X | XS] F ->
  (do
   (F [X|Y])
   (every-cell-helper-x Y XS F)))
(define every-cell-helper-y
  [] F -> done
  [Y | YS] F ->
  (do
   (every-cell-helper-x Y (iota (value *width*)) F)
   (every-cell-helper-y YS F)))
(define every-cell F -> (every-cell-helper-y (iota (value *height*)) F))

(set *graph* (shen.dict 10000)) \* keys are [[X|Y]|Dir] *\
(define add-node [C|Dir] ->
  (let New (vec-add C (dir-offset Dir))
    (put
     [C|Dir] edges
     (append
      (if (not (map-lookup New)) [(cons [New|Dir] 1)] [])
      [(cons [C|(dir-cw Dir)] 1000)
       (cons [C|(dir-ccw Dir)] 1000)])
     (value *graph*))))

(define mapcat
  _ [] -> []
  F [X|XS] -> (append (F X) (mapcat F XS)))
(set
 *all-nodes*
  (mapcat
   (lambda X
     (mapcat
      (lambda Y
        (map
         (lambda Dir
           [[X|Y]|Dir])
         [north south west east]))
      (iota (value *height*))))
   (iota (value *width*))))
(define mapc
  _ [] -> done
  F [X|XS] -> (do (F X) (mapc F XS)))
(mapc (fn add-node) (value *all-nodes*))
"populated graph!"

(define visited? C -> (trap-error (get C visited (value *graph*)) (lambda _ false)))
(define set-visited C -> (put C visited true (value *graph*)))
(define pick-closest-helper
  Cur (cons [_ | -1] Dists) -> (pick-closest-helper Cur Dists)
  [CurC | CurDist] (cons [C | Dist] Dists) ->
  (if (and (or (= CurDist -1) (< Dist CurDist)) (not (visited? C)))
      (pick-closest-helper [C | Dist] Dists)
      (pick-closest-helper [CurC | CurDist] Dists))
  Acc [] -> Acc)
(define pick-closest Dists -> (pick-closest-helper [[-1|-1]|-1] Dists))
(define neighbors C -> (get C edges (value *graph*)))
(define maybe-update BaseDist Neighbors [C | Dist] ->
  (let N (assoc C Neighbors)
    (if (empty? N)
        [C | Dist]
        (let New (+ BaseDist (tl N))
          (if (or (= Dist -1) (< New Dist))
              [(hd N) | New]
              [C | Dist])))))
(print hi) (nl)
(define search-helper S E Dists ->
  (let Close (pick-closest Dists)
    (do
     (print Close) (nl)
     (if (= (tl Close) -1)
         Dists
         (let Neighbors (neighbors (hd Close))
           (do
            (set-visited (hd Close))
            (search-helper S E (map (maybe-update (tl Close) Neighbors) Dists))))))))
(define search ->
  (search-helper
   (value *start*)
   (value *end*)
   (map
    (lambda C
      (if (= C [(value *start*)|east])
          (cons C 0)
          (cons C -1)))
    (value *all-nodes*))))
(set *distances* (search))
"searched graph!"

(define shortest-to-end-helper
  Acc [] -> Acc
  Acc [Dir | Dirs] ->
  (let Res (assoc (cons (value *end*) Dir) (value *distances*))
    (if (empty? Res)
        (shortest-to-end-helper Acc Dirs)
        (if (or (= Acc -1) (< (tl Res) Acc))
            (shortest-to-end-helper (tl Res) Dirs)
            (shortest-to-end-helper Acc Dirs)))))
(define shortest-to-end -> (shortest-to-end-helper -1 [north south west east]))

(define filter _ [] -> [] F [X|XS] -> (if (F X) (cons X (filter F XS)) (filter F XS)))
  
(define label-best-path
  0 _ -> done
  _ [] -> lost
  V Frontier ->
  (do
   (print V)
   (mapc
    (lambda C
      (put (hd C) best true (value *map*)))
    Frontier)
   (let New (mapcat (lambda N (map (fn hd) (neighbors N))) Frontier)
     (do
      (print New) (nl)
      (label-best-path
       (- V 1)
       (filter
        (lambda C
          (let Res (tl (assoc C (value *distances*)))
            (do
             (print Res) (nl)
             (or (= (- V 1) Res) (= (- V 1000) Res)))))
        New))))))
(label-best-path (shortest-to-end) [[(value *end*)|south]])

(define sum-best ->
  (sum
   (map
    (lambda C
      (if (trap-error (get C best (value *map*)) (lambda _ false)) 1 0))
    (mapcat (lambda X (map (lambda Y [X|Y]) (iota (value *height*)))) (iota (value *width*))))))
