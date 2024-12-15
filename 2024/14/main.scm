(use-modules (srfi srfi-1))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 match))

(define (split delim str)
  (let ((len (string-length str))
        (dlen (string-length delim)))
    (define (sp acc s i)
      (let ((dend (+ i dlen)))
        (cond
         ((> i len) (cons (substring str s len) acc))
         ((and (<= dend len) (string=? (substring str i dend) delim))
          (sp (cons (substring str s i) acc) dend dend))
         (#t
          (sp acc s (+ i 1))))))
    (reverse (sp '() 0 0))))

(define (list->pair l)
  (cons (car l) (cadr l)))

(define (get-input)
  (let* ((inp (call-with-input-file "2024/14/input.txt" get-string-all))
         (lines (filter (lambda (x) (not (string-null? x))) (split "\n" inp)))
         (pv
          (map
           (lambda (l)
             (list->pair
              (map
               (lambda (e)
                 (list->pair (map string->number (split "," (substring e 2)))))
               (split " " l))))
           lines)))
    pv))

(define (move-robot r)
  (match r
    (((x . y) . (vx . vy))
     (let ((nx (modulo (+ x vx) width))
           (ny (modulo (+ y vy) height)))
       (cons (cons nx ny) (cons vx vy)))
     )))

(define (move-robot-n n r)
  (if (= n 0) r (move-robot-n (- n 1) (move-robot r))))

(define (quadrant r)
  (let ((midw (quotient width 2))
        (midh (quotient height 2)))
  (match r
    (((x . y) . _)
     (cond
      ((and (< midw x) (< midh y)) 'nw)
      ((and (> midw x) (< midh y)) 'ne)
      ((and (< midw x) (> midh y)) 'sw)
      ((and (> midw x) (> midh y)) 'se)
      (#t '()))))))

;; (define width 11)
;; (define height 7)
(define width 101)
(define height 103)

(define (p1)
  (let* ((rs (get-input))
         (final (map (lambda (r) (move-robot-n 100 r)) rs))
         (quads '(nw ne sw se))
         (quadbots (map (lambda (q) (filter (lambda (r) (eqv? q (quadrant r))) final)) quads)))
    (fold * 1 (map length quadbots))))

(define (p2)
  (define (loop f cnt rs)
    (let ((frame (string-append "Iteration: " (number->string cnt) "\n")))
      (display frame)
      (for-each
       (lambda (y)
         (for-each
          (lambda (x)
            (let ((r (assoc (cons x y) rs)))
              (set! frame (string-append frame (if r "#" ".")))))
          (iota width))
         (set! frame (string-append frame "\n")))
       (iota height))
      (display frame f)
      (when (< cnt 10000)
        (loop f (+ cnt 1) (map move-robot rs)))))
  (call-with-output-file "2024/14/out.txt"
    (lambda (f)
      (loop f 0 (get-input)))))

(p2)
