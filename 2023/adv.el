;;; adv --- Advent of Code reference implementations -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'ht)
(require 'f)
(require 'cl-lib)

;; Helpers
(defun adv/slurp (path)
  "Read PATH and return a string."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun adv/lines (path)
  "Read PATH and return a list of nonempty lines."
  (-map #'s-trim (-filter #'s-present? (s-lines (adv/slurp path)))))

(defun adv/cat (xs)
  "Concatenate a list of lists XS."
  (apply #'append xs))

(defun adv/range (start end)
  "Create a list of all elements from START (inclusive) to END (exclusive)."
  (-iota (- end start) start))

(defun adv/@ (i xs)
  "Return the value of XS at I."
  (cond
   ((sequencep xs) (seq-elt xs i))
   (t nil)))

(defun adv/all-adjacent (c)
  "Return all of the points adjacent to C."
  (let ((x (car c))
        (y (cdr c)))
    (list
     (cons (- x 1) y)
     (cons (+ x 1) y)
     (cons x (- y 1))
     (cons x (+ y 1)))))
(defun adv/adjacent (c1 c2)
  "Return non-nil if coordinates C1 are adjacent to C2."
  (let ((x1 (car c1)) (y1 (cdr c1))
        (x2 (car c2)) (y2 (cdr c2)))
    (and
     (>= x2 (- x1 1))
     (<= x2 (+ x1 1))
     (>= y2 (- y1 1))
     (<= y2 (+ y1 1)))))

(defun adv/grid (path)
  "Read PATH and return a grid of characters."
  (let* ((lines (adv/lines path))
         (h (length lines))
         (w (-max (-map #'length lines))))
    (list w h lines)))
(defun adv/g-clone (g)
  "Copy G."
  (let ((lines (-map #'copy-sequence (caddr g))))
    (list (car g) (cadr g) lines)))
(defun adv/g-w (g) "Return the width of G." (car g))
(defun adv/g-h (g) "Return the height of G." (cadr g))
(defun adv/g-str (g) "Return the contents of G." (caddr g))
(defun adv/g (g x y &optional def)
  "Retrieve the character at X, Y in G.
Return DEF or nil if not present."
  (cond
   ((< x 0) def)
   ((< y 0) def)
   ((>= x (adv/g-w g)) def)
   ((>= y (adv/g-h g)) def)
   (t (or (adv/@ x (adv/@ y (adv/g-str g))) def))))
(defun adv/g-set (g x y val)
  "Set the character at X, Y in G to VAL."
  (cond
   ((< x 0) nil)
   ((< y 0) nil)
   ((>= x (adv/g-w g)) nil)
   ((>= y (adv/g-h g)) nil)
   (t (setf (seq-elt (seq-elt (adv/g-str g) y) x) val))))
(defun adv/g-inbounds (g coords)
  "Return non-nil if COORDS are in-bounds in G."
  (let ((x (car coords))
        (y (cdr coords)))
    (and
     (>= x 0)
     (< x (adv/g-w g))
     (>= y 0)
     (< y (adv/g-h g)))))
(defun adv/g-xy (g coords &optional def)
  "Retrieve the character at COORDS in G.
Return DEF or nil if not present."
  (adv/g g (car coords) (cdr coords) def))
(defun adv/g-print (g)
  "Return a printable representation of G."
  (print (format "\n%s" (s-join "\n" (adv/g-str g)))))

(defun adv/first2 (lst)
  "Return a pair of the first two elements of LST."
  (cons (car lst) (cadr lst)))

(defun adv/str (x)
  "Convert X to a string."
  (format "%s" x))

(defun adv/num (x)
  "Convert X to a number."
  (cond
   ((numberp x) x)
   ((stringp x) (string-to-number (s-trim x)))
   (t (error "Failed to convert %s to a number" x))))

(defun adv/s@ (k m)
  "Lookup the value associated with K in the alist M."
  (alist-get k m nil nil #'s-equals?))

(defun adv/@num (k m)
  "Lookup the number associated with K in the alist M."
  (adv/num (alist-get k m "0" nil #'equal)))


;; Day 1
(defconst adv/day1-digit-values
  '(("0" . 0)
    ("1" .  1) ("one" . 1)
    ("2" .  2) ("two" . 2)
    ("3" .  3) ("three" . 3)
    ("4" .  4) ("four" . 4)
    ("5" .  5) ("five" . 5)
    ("6" .  6) ("six" . 6)
    ("7" .  7) ("seven" . 7)
    ("8" .  8) ("eight" . 8)
    ("9" .  9) ("nine" . 9)))
(defun adv/day1-find-digit (cmp line)
  "Find a digit tehe CMP and LINE are involved somehow please guess."
  (cdr
   (-min-by
    (-on cmp #'car)
    (-filter
     #'car
     (apply
      #'-concat
      (--map
       (-map
        (lambda (ma) (cons (car ma) (cdr it)))
        (s-matched-positions-all (car it) line))
       adv/day1-digit-values))))))
(defun adv/day1-line-value (line)
  "Return the value for LINE :3."
  (+
   (* 10 (adv/day1-find-digit #'>= line))
   (adv/day1-find-digit #'<= line)))
(defun adv/day1 ()
  "Solve Advent of Code 2023 Day 1."
  (let ((lines (adv/lines "1/input.txt")))
    (-sum (-map #'adv/day1-line-value lines))))

;; Day 2
(defun adv/day2-parse-game (l)
  "Parse L into a game."
  (let* ((gsp (s-split ": " l))
         (idx (adv/num (s-chop-prefix "Game " (car gsp))))
         (set-strs (s-split "; " (cadr gsp)))
         (sets (--map (-map (lambda (v) (adv/first2 (reverse (s-split " " v)))) (s-split ", " it)) set-strs)))
    (cons idx sets)))
(defun adv/day2-valid-game (g)
  "Return non-nil if G exceed 12 red, 13 green, or 14 blue."
  (--all?
   (and
    (<= (adv/@num "red" it) 12)
    (<= (adv/@num "green" it) 13)
    (<= (adv/@num "blue" it) 14))
   (cdr g)))
(defun adv/day2-game-power (g)
  "Compute the power of G."
  (let ((red (-max (--map (adv/@num "red" it) (cdr g))))
        (green (-max (--map (adv/@num "green" it) (cdr g))))
        (blue (-max (--map (adv/@num "blue" it) (cdr g)))))
    (* red green blue)))
(defun adv/day2-part1 ()
  "Solve day 2 part 1."
  (let* ((lines (adv/lines "2/input.txt"))
         (games (-map #'adv/day2-parse-game lines))
         (valid-games (-filter #'adv/day2-valid-game games)))
    (-sum (-map #'car valid-games))))
(defun adv/day2-part2 ()
  "Solve day 2 part 1."
  (let* ((lines (adv/lines "2/input.txt"))
         (games (-map #'adv/day2-parse-game lines)))
    (-sum (-map #'adv/day2-game-power games))))

;; Day 3
(defun adv/day3-symbol-coords (g)
  "Return the coordinates of all symbols in G."
  (-non-nil
   (adv/cat
    (-map
     (lambda (x)
       (-map
        (lambda (y)
          (when (-contains? '(?! ?@ ?# ?$ ?% ?^ ?& ?* ?- ?+ ?= ?/) (adv/g g x y))
            (cons x y)))
        (-iota (adv/g-h g))))
     (-iota (adv/g-w g))))))
(defun adv/day3-gear-coords (g)
  "Return the coordinates of all symbols in G."
  (-non-nil
   (adv/cat
    (-map
     (lambda (x)
       (-map
        (lambda (y)
          (when (-contains? '(?*) (adv/g g x y))
            (cons x y)))
        (-iota (adv/g-h g))))
     (-iota (adv/g-w g))))))
(defun adv/day3-part1 ()
  "Solve day 3 part 1."
  (let* ((g (adv/grid "3/input.txt"))
         (symbols (adv/day3-symbol-coords g))
         (lines (adv/g-str g))
         (num-coords
          (adv/cat
           (--map-indexed
            (-map
             (lambda (m)
               (cons
                (adv/num (substring it (car m) (cdr m)))
                (-map (lambda (x) (cons x it-index)) (adv/range (car m) (cdr m)))))
             (s-matched-positions-all (rx (one-or-more digit)) it))
            lines)))
         (matching-nums
          (--filter
           (-any?
            (lambda (nc)
              (-any?
               (lambda (sc)
                 (adv/adjacent nc sc))
               symbols))
            (cdr it))
           num-coords))
         )
    (-sum (-map #'car matching-nums))))
(defun adv/day3-part2 ()
  "Solve day 3 part 2."
  (let* ((g (adv/grid "3/input.txt"))
         (lines (adv/g-str g))
         (num-coords
          (adv/cat
           (--map-indexed
            (-map
             (lambda (m)
               (cons
                (adv/num (substring it (car m) (cdr m)))
                (-map (lambda (x) (cons x it-index)) (adv/range (car m) (cdr m)))))
             (s-matched-positions-all (rx (one-or-more digit)) it))
            lines)))
         (pregears
          (--map
           (cons it (-filter (lambda (n) (-any? (lambda (nc) (adv/adjacent it nc)) (cdr n))) num-coords))
           (adv/day3-gear-coords g)))
         (gears (--filter (= 2 (length (cdr it))) pregears))
         (ratios (--map (* (caadr it) (caaddr it)) gears))
         )
    (-sum ratios)))

;; Day 4
(defun adv/day4-parse-card (l)
  "Parse L into a card."
  (let* ((sp (s-split ": " l))
         (idx (adv/num (s-chop-prefix "Card " (car sp))))
         (nsp (s-split " | " (cadr sp)))
         (winning (-map #'adv/num (-filter #'s-present? (s-split " " (car nsp)))))
         (ours (-map #'adv/num (-filter #'s-present? (s-split " " (cadr nsp))))))
    (list idx winning ours)))
(defun adv/day4-part1 ()
  "Solve day 4 part 1."
  (let* ((lines (adv/lines "4/input.txt"))
         (cards (-map #'adv/day4-parse-card lines))
         (card-wins (--map (cons (car it) (-filter (lambda (our) (-contains? (cadr it) our)) (caddr it))) cards))
         (card-values
          (--map
           (let ((v (length (cdr it))))
             (cons (car it) (if (= 0 v) 0 (expt 2 (- v 1)))))
           card-wins))
         )
    (-sum (-map #'cdr card-values))))
(defun adv/day4-part2 ()
  "Solve day 4 part 2."
  (let* ((lines (adv/lines "4/input.txt"))
         (cards (-map #'adv/day4-parse-card lines))
         (card-wins (--map (cons (car it) (-filter (lambda (our) (-contains? (cadr it) our)) (caddr it))) cards))
         (card-copies (--map (cons (car it) (adv/range (+ (car it) 1) (+ (car it) 1 (length (cdr it))))) card-wins))
         (card-counts (ht-from-alist (--map (cons (car it) 1) card-copies))))
    (--each card-copies
      (let ((this-copies (ht-get card-counts (car it))))
        (-each (cdr it)
          (lambda (copy)
            (let ((old (ht-get card-counts copy)))
              (ht-set! card-counts copy (+ old this-copies)))))))
    (-sum (ht-values card-counts))))

;; Day 5
(defun adv/day5-parse-map (str)
  "Parse a range map from STR."
  (let* ((lines (cdr (s-split "\n" (s-trim str))))
         (ranges
          (--map
           (-map #'adv/num (s-split " " it))
           lines)))
    ranges))
(defun adv/day5-lookup (x map)
  "Lookup X in MAP."
  (let ((range (--first (and (>= x (cadr it)) (< x (+ (cadr it) (caddr it)))) map)))
    (if range
        (+ (car range) (- x (cadr range)))
      x)))
(defun adv/day5-part1 ()
  "Solve day 5 part 1."
  (let* ((inp (adv/slurp "5/input.txt"))
         (blocks (s-split "\n\n" inp))
         (seeds (-map #'adv/num (s-split " " (s-chop-prefix "seeds: " (car blocks)))))
         (maps (-map #'adv/day5-parse-map (cdr blocks)))
         (stl (lambda (s) (--reduce-from (adv/day5-lookup acc it) s maps)))
         (locations (-map stl seeds)))
    (-min locations)))
(defun adv/day5-range-intersection (s0 e0 s1 e1)
  "Return a range of shared elements of S0 <= x < E0 and S1 <= x < E1."
  (let ((start
         (cond
          ((and (<= s0 s1) (< s1 e0)) s1)
          ((and (<= s1 s0) (< s0 e1)) s0)
          (t nil)))
        (end (min e0 e1)))
    (when start (cons start end))))
(defun adv/day5-range-difference (s0 e0 s1 e1)
  "Return the difference of S0 <= x < E0 and S1 <= x < E1."
  (cond
   ((and (>= s0 s1) (<= e0 e1)) nil)
   ((and (< s0 s1) (> e0 e1)) (list (cons s0 s1) (cons e1 e0)))
   ((and (>= e0 s1) (<= e0 e1)) (list (cons s0 s1)))
   ((and (>= s0 s1) (<= s0 e1)) (list (cons e1 e0)))
   (t (list (cons s0 e0)))))
(defun adv/day5-translate-range (r map)
  "Translate R over MAP."
  (let ((translated
         (-non-nil
          (-map
           (lambda (d)
             (let ((i (adv/day5-range-intersection
                       (car r) (cdr r)
                       (cadr d) (+ (cadr d) (caddr d)))))
               (when i
                 (cons
                  (+ (car d) (- (car i) (cadr d)))
                  (+ (car d) (- (cdr i) (cadr d))))))
             )
           map)))
        (leftovers
         (--reduce-from
          (-non-nil (adv/cat (-map (lambda (l) (adv/day5-range-difference (car l) (cdr l) (cadr it) (+ (cadr it) (caddr it)))) acc)))
          (list r)
          map))
        )
    (append translated leftovers)
    ))
(defun adv/day5-translate-all (r maps)
  "Translate R over MAPS."
  (--reduce-from (adv/cat (-map (lambda (i) (adv/day5-translate-range i it)) acc)) (list r) maps))
(defun adv/day5-part2 ()
  "Solve day 5 part 2."
  (let* ((inp (adv/slurp "5/input.txt"))
         (blocks (s-split "\n\n" inp))
         (seeds (-partition 2 (-map #'adv/num (s-split " " (s-chop-prefix "seeds: " (car blocks))))))
         (maps (-map #'adv/day5-parse-map (cdr blocks)))
         (locations (-non-nil (adv/cat (--map (adv/day5-translate-all (cons (car it) (+ (car it) (cadr it))) maps) seeds)))))
    (-min (-map #'car locations))))

;; Day 6
(defun adv/day6-recordbeaters (r)
  "Compute the recordbeating button press times for R."
  (--filter (> it (cdr r)) (--map (* it (- (car r) it)) (-iota (car r)))))
(defun adv/day6-part1 ()
  "Solve day 6 part 1."
  (let* ((inp (adv/lines "6/input.txt"))
         (times (-map #'adv/num (s-split " " (s-trim (s-chop-prefix "Time:" (car inp))) t)))
         (distances (-map #'adv/num (s-split " " (s-trim (s-chop-prefix "Distance:" (cadr inp))) t)))
         (td (-zip-pair times distances))
         (recordbeaters (-map #'adv/day6-recordbeaters td)))
    (-reduce #'* (-map #'length recordbeaters))))
(defun adv/day6-recordbeaters-fast (r)
  "Compute the number of recordbeating button press times for R."
  (let* ((b (car r))
         (a (cdr r))
         (bound0 (floor (/ (- b (sqrt (- (* b b) (* 4 a)))) 2)))
         (bound1 (floor (/ (+ b (sqrt (- (* b b) (* 4 a)))) 2)))
         (lower (min bound0 bound1))
         (upper (max bound0 bound1)))
    (- upper lower)))
(defun adv/day6-part2 ()
  "Solve day 6 part 2."
  (let* ((inp (adv/lines "6/input.txt"))
         (time (adv/num (s-replace " " "" (s-trim (s-chop-prefix "Time:" (car inp))))))
         (distance (adv/num (s-replace " " "" (s-trim (s-chop-prefix "Distance:" (cadr inp)))))))
    (adv/day6-recordbeaters-fast (cons time distance))))

;; Day 7
(defun adv/day7-card-value (c)
  "Determine the value of a card C."
  (cl-case c
    (?2 2)
    (?3 3)
    (?4 4)
    (?5 5)
    (?6 6)
    (?7 7)
    (?8 8)
    (?9 9)
    (?T 10)
    (?J 11)
    (?Q 12)
    (?K 13)
    (?A 14)
    (otherwise 0)))
(defun adv/day7-hand-type (h)
  "Determine the type index of a hand string H."
  (let* ((hl (seq-into h 'list))
         (card-indices (--map (cons it (-elem-indices it hl)) (seq-into "23456789TJQKA" 'list))))
    (cond
     ((= 1 (length (-uniq hl))) 7)
     ((= 4 (-max (--map (length (cdr it)) card-indices))) 6)
     ((= 2 (length (-uniq hl))) 5)
     ((= 3 (-max (--map (length (cdr it)) card-indices))) 4)
     ((= 3 (length (-uniq hl))) 3)
     ((= 2 (-max (--map (length (cdr it)) card-indices))) 2)
     (t 1))))
(defun adv/day7-hand-score (h)
  "Compute a complete score for H."
  (+
   (* (adv/day7-hand-type h) 10e9)
   (* (adv/day7-card-value (seq-elt h 0)) 10e7)
   (* (adv/day7-card-value (seq-elt h 1)) 10e5)
   (* (adv/day7-card-value (seq-elt h 2)) 10e3)
   (* (adv/day7-card-value (seq-elt h 3)) 10e1)
   (adv/day7-card-value (seq-elt h 4))
   ))
(defun adv/day7-part1 ()
  "Solve day 7 part 1."
  (let* ((lines (adv/lines "7/input.txt"))
         (handbids (--map (let ((sp (s-split " " it))) (cons (car sp) (adv/num (cadr sp)))) lines))
         (sorted (-sort (-on #'<= (lambda (x) (adv/day7-hand-score (car x)))) handbids))
         (valued (--map-indexed (* (cdr it) (+ it-index 1)) sorted))
         )
    (-sum valued)
    ))
(defun adv/day7-replace-jokers (h)
  "Replace jokers in H."
  (let* ((hl (seq-into h 'list))
         (card-indices (--filter (not (= (car it) ?J)) (--map (cons it (length (-elem-indices it hl))) (seq-into "23456789TJQKA" 'list))))
         (sorted-card-indices (-sort (-on #'>= #'cdr) card-indices))
         (max-occurences (--filter (= (cdr it) (cdar sorted-card-indices)) sorted-card-indices))
         (sorted-max (-sort (-on #'>= (lambda (x) (adv/day7-card-value (car x)))) max-occurences))
         )
    (s-replace "J" (format "%c" (caar sorted-max)) h)))
(defun adv/day7-card-value-joker (c)
  "Determine the value of a card C taking into account jokers."
  (if (= c ?J) 1 (adv/day7-card-value c)))
(defun adv/day7-hand-score-joker (h)
  "Compute a complete score for H taking into account jokers."
  (let ((hj (adv/day7-replace-jokers h)))
    (+
     (* (adv/day7-hand-type hj) 10e9)
     (* (adv/day7-card-value-joker (seq-elt h 0)) 10e7)
     (* (adv/day7-card-value-joker (seq-elt h 1)) 10e5)
     (* (adv/day7-card-value-joker (seq-elt h 2)) 10e3)
     (* (adv/day7-card-value-joker (seq-elt h 3)) 10e1)
     (adv/day7-card-value-joker (seq-elt h 4))
     )))
(defun adv/day7-part2 ()
  "Solve day 7 part 2."
  (let* ((lines (adv/lines "7/input.txt"))
         (handbids (--map (let ((sp (s-split " " it))) (cons (car sp) (adv/num (cadr sp)))) lines))
         (sorted (-sort (-on #'<= (lambda (x) (adv/day7-hand-score-joker (car x)))) handbids))
         (valued (--map-indexed (* (cdr it) (+ it-index 1)) sorted))
         )
    (-sum valued)
    ))

;; Day 8
(defun adv/day8-count-steps (ins nodes cur)
  "Walk INS over NODES from CUR."
  (let ((ret 0))
    (while (not (s-equals? cur "ZZZ"))
      (cond
       ((= (car ins) ?L)
        (setf ins (append (cdr ins) (list (car ins))))
        (setf cur (car (adv/s@ cur nodes)))
        (cl-incf ret))
       ((= (car ins) ?R)
        (setf ins (append (cdr ins) (list (car ins))))
        (setf cur (cdr (adv/s@ cur nodes)))
        (cl-incf ret))))
    ret))
(defun adv/day8-part1 ()
  "Solve day 8 part 1."
  (let* ((lines (adv/lines "8/input.txt"))
         (instructions (seq-into (car lines) 'list))
         (nodes
          (--map
           (let* ((sp (s-split " = " (s-replace-all '(("(" . "") (")" . "")) it)))
                  (node (s-trim (car sp)))
                  (spe (s-split ", " (cadr sp)))
                  (edges (cons (car spe) (cadr spe))))
             (cons node edges))
           (cdr lines)
           )))
    (adv/day8-count-steps instructions nodes "AAA")
    ))
(defun adv/day8-one-step (ins nodes cur)
  "Walk one INS over NODES from CUR."
  (cond
   ((= ins ?L)
    (car (adv/s@ cur nodes)))
   ((= ins ?R)
    (cdr (adv/s@ cur nodes)))))
(defun adv/day8-walk-all (ins nodes allcur)
  "Walk INS over NODES from ALLCUR."
  (let ((ret 0)
        (arr (apply #'vector allcur))
        (all (seq-map (lambda (_) nil) (apply #'vector allcur))))
    (while (not (--all? it (seq-into all 'list)))
      (let ((sel (cond ((= (car ins) ?L) #'car) ((= (car ins) ?R) #'cdr))))
        (seq-do-indexed
         (lambda (x idx)
           (when (and (= (seq-elt x 2) ?Z) (not (seq-elt all idx)))
             (setf (seq-elt all idx) ret))
           (setf (seq-elt arr idx) (funcall sel (adv/s@ x nodes))))
         arr)
        (setf ins (append (cdr ins) (list (car ins))))
        (cl-incf ret)))
    (apply #'cl-lcm (seq-into all 'list))))
(defun adv/day8-part2 ()
  "Solve day 8 part 2."
  (let* ((lines (adv/lines "8/input.txt"))
         (instructions (seq-into (car lines) 'list))
         (nodes
          (--map
           (let* ((sp (s-split " = " (s-replace-all '(("(" . "") (")" . "")) it)))
                  (node (s-trim (car sp)))
                  (spe (s-split ", " (cadr sp)))
                  (edges (cons (car spe) (cadr spe))))
             (cons node edges))
           (cdr lines)
           )))
    (adv/day8-walk-all instructions nodes (-map #'car (--filter (= (seq-elt (car it) 2) ?A) nodes)))
    ))

;; Day 9
(defun adv/day9-seq-diffs (seq)
  "Return the difference between each element of SEQ."
  (cond
   ((and (car seq) (cadr seq)) (cons (- (cadr seq) (car seq)) (adv/day9-seq-diffs (cdr seq))))
   (t nil)))
(defun adv/day9-seq-done (seq)
  "Return non-nil if SEQ is all zeroes."
  (--all? (= it 0) seq))
(defun adv/day9-explode-seq (seq)
  "Explode SEQ."
  (let ((ret (list seq)))
    (while (not (adv/day9-seq-done (car ret)))
      (push (adv/day9-seq-diffs (car ret)) ret))
    ret))
(defun adv/day9-extrapolate (seqs)
  "Sum all of the last elements of SEQS."
  (-sum (-map #'-last-item seqs)))
(defun adv/day9-extrapolate-backwards (seqs)
  "Difference all of the first elements of SEQS."
  (--reduce (- it acc) (-map #'car seqs)))
(defun adv/day9-part1 ()
  "Solve day 9 part 1."
  (let* ((lines (adv/lines "9/input.txt"))
         (seqs (--map (-map #'adv/num (s-split " " it)) lines))
         (exploded (-map #'adv/day9-explode-seq seqs))
         (extrapolated (-map #'adv/day9-extrapolate exploded)))
    (-sum extrapolated)))
(defun adv/day9-part2 ()
  "Solve day 9 part 2."
  (let* ((lines (adv/lines "9/input.txt"))
         (seqs (--map (-map #'adv/num (s-split " " it)) lines))
         (exploded (-map #'adv/day9-explode-seq seqs))
         (extrapolated (-map #'adv/day9-extrapolate-backwards exploded)))
    (-sum extrapolated)))

;; Day 10 - pretty scuffed, I guessed a bit
(defun adv/day10-start (grid)
  "Return the coordinates of S in GRID."
  (car
   (-non-nil
    (adv/cat
     (-map
      (lambda (x)
        (-map
         (lambda (y)
           (when (equal ?S (adv/g grid x y))
             (cons x y)))
         (-iota (adv/g-h grid))))
      (-iota (adv/g-w grid)))))))
(defun adv/day10-connected (grid c)
  "Return the coordinates of all cells connected to C on GRID."
  (let ((x (car c))
        (y (cdr c)))
    (cl-case (adv/g-xy grid c)
      (?| (list (cons x (- y 1)) (cons x (+ y 1))))
      (?- (list (cons (- x 1) y) (cons (+ x 1) y)))
      (?L (list (cons x (- y 1)) (cons (+ x 1) y)))
      (?J (list (cons (- x 1) y) (cons x (- y 1))))
      (?7 (list (cons (- x 1) y) (cons x (+ y 1))))
      (?F (list (cons x (+ y 1)) (cons (+ x 1) y)))
      (?S
       (--filter
        (and
         (adv/g-inbounds grid it)
         (-contains? (adv/day10-connected grid it) c))
        (list
         (cons (- x 1) y)
         (cons (+ x 1) y)
         (cons x (- y 1))
         (cons x (+ y 1)))))
      (t nil))))
(defun adv/day10-build-graph (grid start)
  "Construct a graph of the loop on GRID from START."
  (let ((queue (list start))
        (ret nil)
        (covered nil))
    (while (car queue)
      (let ((conn (adv/day10-connected grid (car queue))))
        (push (cons (car queue) conn) ret)
        (push (car queue) covered)
        (setq queue (cdr queue))
        (--each conn
          (unless (or (-contains? queue it) (-contains? covered it))
            (push it queue)))))
    ret))
(defun adv/day10-bfs (graph start)
  "Return an alist mappign each node in GRAPH to its distance from START."
  (let ((queue (list start))
        (distances (--map (cons (car it) nil) graph)))
    (setf (alist-get start distances nil nil #'equal) 0)
    (while (car queue)
      (let ((e (car queue)))
        (setq queue (cdr queue))
        (--each (alist-get e graph nil nil #'equal)
          (unless (alist-get it distances nil nil #'equal)
            (setf
             (alist-get it distances nil nil #'equal)
             (+ 1 (alist-get e distances nil nil #'equal)))
            (setq queue (append queue (list it)))))))
    distances
    ))
(defun adv/day10-right-of (prev cur)
  "Return the point to the left of CUR, assuming the previous point PREV."
  (let ((dx (- (car cur) (car prev)))
        (dy (- (cdr cur) (cdr prev))))
    (cons (- (car cur) dy) (+ (cdr cur) dx))))
(defun adv/day10-walk-clockwise (graph start)
  "Walk the path of GRID and GRAPH from START clockwise.
Return the list of nodes visited in order."
  (let ((ret (list start))
        (next (adv/day10-cw-from graph start)))
    (while (not (-contains? ret next))
      (let* ((conns (alist-get next graph nil nil #'equal))
             (other (car (--filter (not (equal (car ret) it)) conns))))
        (push next ret)
        (setq next other)))
    (reverse ret)))
(defun adv/day10-flood (grid walls starting)
  "Flood fill from STARTING on GRID, bounded by WALLS."
  (let ((queue starting)
        (visited starting))
    (while (car queue)
      (let ((e (car queue)))
        (setq queue (cdr queue))
        (--each
            (--filter
             (and
              (>= (car it) 0)
              (>= (cdr it) 0)
              (< (car it) (adv/g-w grid))
              (< (cdr it) (adv/g-h grid))
              (not (-contains? walls it)))
             (adv/all-adjacent e))
          (unless (-contains? visited it)
            (push it visited)
            (setq queue (append queue (list it)))))))
    visited))
(defun adv/day10-indicate-points (grid points &optional char)
  "Indicate POINTS on GRID with CHAR."
  (let ((g (adv/g-clone grid)))
    (--each points
      (adv/g-set g (car it) (cdr it) (or char ?#)))
    g))
(defun adv/day10-part1 ()
  "Solve day 10 part 1."
  (let* ((grid (adv/grid "10/input.txt"))
         (start (adv/day10-start grid))
         (graph (adv/day10-build-graph grid start))
         (distances (adv/day10-bfs graph start))
         )
    (-max (-map #'cdr distances))
    ))
(defun adv/day10-cw-from (graph p)
  "Return the point proceeding clockwise from P on GRID and GRAPH."
  (let* ((conns (alist-get p graph nil nil #'equal)))
    (cadr conns)))
(defun adv/day10-part2 ()
  "Solve day 10 part 2."
  (let* ((grid (adv/grid "10/input.txt"))
         (start (adv/day10-start grid))
         (graph (adv/day10-build-graph grid start))
         (path (adv/day10-walk-clockwise graph start))
         (right (--map (adv/day10-right-of (car it) (cdr it)) (-zip-pair path (append (cdr path) (list start)))))
         (seeds (-difference right path))
         (grown (adv/day10-flood grid path seeds))
         )
    (adv/g-print (adv/day10-indicate-points grid grown))
    (length grown)
    ))

;; Day 11
(defun adv/day11-expand-rows (rows)
  "Expand ROWS."
  (adv/cat
   (--map
    (if (-all? (lambda (c) (= c ?.)) it)
        (list it it)
      (list it))
    rows)))
(defun adv/day11-expand-columns (rows)
  "Expand columns in ROWS."
  (apply
   #'-zip-lists
   (adv/cat
    (--map
     (if (-all? (lambda (c) (= c ?.)) it)
         (list it it)
       (list it))
     (apply #'-zip-lists rows)))))
(defun adv/day11-pairs (xs)
  "Return all of the unique pairs from elements in XS."
  (if (car xs)
      (append (--map (cons (car xs) it) (cdr xs)) (adv/day11-pairs (cdr xs)))
    nil))
(defun adv/day11-distance (p1 p2)
  "Return the Manhattan distance between P1 and P2."
  (let ((dx (- (car p1) (car p2)))
        (dy (- (cdr p1) (cdr p2))))
    (+ (abs dx) (abs dy))))
(defun adv/day11-part1 ()
  "Solve day 11 part 1."
  (let* ((grid (adv/grid "11/input.txt"))
         (rows
          (adv/day11-expand-columns
           (adv/day11-expand-rows
            (--map
             (seq-into it 'list)
             (adv/g-str grid)))))
         (galaxies
          (-non-nil
           (adv/cat
            (-map-indexed
             (lambda (y row)
               (-map-indexed
                (lambda (x c)
                  (if (= c ?#)
                      (cons x y)
                    nil)
                  )
                row))
             rows))))
         )
    (-sum (--map (adv/day11-distance (car it) (cdr it)) (adv/day11-pairs galaxies)))))
(defun adv/day11-find-rows (rows)
  "Return the indices of the empty ROWS."
  (-non-nil
   (--map-indexed
    (when (-all? (lambda (c) (= c ?.)) it)
      it-index)
    rows)))
(defun adv/day11-find-columns (rows)
  "Return the indices of the empty columns in ROWS."
  (-non-nil
   (--map-indexed
    (when (-all? (lambda (c) (= c ?.)) it)
      it-index)
    (apply #'-zip-lists rows))))
(defun adv/day11-convert-point (rows cols p)
  "Convert P given expanded ROWS and COLS."
  (let ((factor (- 1000000 1)))
    (cons
     (+ (car p) (* factor (length (--filter (< it (car p)) cols))))
     (+ (cdr p) (* factor (length (--filter (< it (cdr p)) rows)))))))
(defun adv/day11-part2 ()
  "Solve day 11 part 2."
  (let* ((grid (adv/grid "11/input.txt"))
         (g (--map (seq-into it 'list) (adv/g-str grid)))
         (rows (adv/day11-find-rows g))
         (cols (adv/day11-find-columns g))
         (galaxies-raw
          (-non-nil
           (adv/cat
            (-map-indexed
             (lambda (y row)
               (-map-indexed
                (lambda (x c)
                  (if (= c ?#)
                      (cons x y)
                    nil)
                  )
                row))
             g))))
         (galaxies (--map (adv/day11-convert-point rows cols it) galaxies-raw))
         )
    (-sum (--map (adv/day11-distance (car it) (cdr it)) (adv/day11-pairs galaxies)))))

(provide 'adv)
;;; adv.el ends here
