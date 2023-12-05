;;; adv --- Advent of Code reference implementations -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'ht)
(require 'f)

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
(defun adv/g-w (g) "Return the width of G." (car g))
(defun adv/g-h (g) "Return the height of G." (cadr g))
(defun adv/g-str (g) "Return the contents of G." (caddr g))
(defun adv/g (g x y &optional def)
  "Retrieve the character at X, Y in G.
Return DEF or nil if not present."
  (cond
   ((>= x (adv/g-w g)) def)
   ((>= y (adv/g-h g)) def)
   (t (or (adv/@ x (adv/@ y (adv/g-str g))) def))))
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

(defun adv/@num (k m)
  "Lookup the number associated with K in the alist M."
  (adv/num (alist-get k m "0" nil #'equal)))

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

;; Day3
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

(provide 'adv)
;;; adv.el ends here
