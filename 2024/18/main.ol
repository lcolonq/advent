(import (owl ff) (owl queue))

(define *inp* (file->string "input.txt"))

;; why is this slow as hell in otus
(define (string-split delim str)
  (let ((len (string-length str))
        (dlen (string-length delim)))
    (define (sp acc s i)
      (print i)
      (let ((dend (+ i dlen)))
        (cond
         ((> i len) (cons (substring str s len) acc))
         ((and (<= dend len) (string=? (substring str i dend) delim))
          (sp (cons (substring str s i) acc) dend dend))
         (#t
          (sp acc s (+ i 1))))))
    (reverse (sp '() 0 0))))

(define (list->pair xs) (cons (car xs) (cadr xs)))

(define *coords*
  (map
   (lambda (x) (list->pair (map string->number (string-split "," x))))
   (filter
    (lambda (x) (> (string-length x) 0))
    (string-split "\n" *inp*))))

(define (open-neighbors c w h g)
  (let ((x (car c)) (y (cdr c)))
    (filter
     (lambda (c)
       (let ((cx (car c)) (cy (cdr c)))
         (not (or (< cx 0) (>= cx w) (< cy 0) (>= cy h) (member c g)))))
     (list
      (cons (- x 1) y)
      (cons (+ x 1) y)
      (cons x (- y 1))
      (cons x (+ y 1))))))

(define (to-key w h k)
  (let ((x (car k)) (y (cdr k)))
    (+ x (* w y))))

(define (gget g w h k d)
  (get g (to-key w h k) d))

(define (gput g w h k v)
  (put g (to-key w h k) v))

(define (solve w h cnt)
  (let ((g (take *coords* cnt)))
    (define (loop oldq weights)
      (cond
       ((qnull? oldq) weights)
       (#t
        (call-with-values (lambda () (quncons oldq #false))
          (lambda (v newq)
            (let ((ns (open-neighbors v w h g)))
              (apply
               loop 
               (fold
                (lambda (acc n)
                  (let* ((q (car acc)) (ws (cadr acc)))
                    (if (gget ws w h n #false)
                        (list q ws)
                        (list (qsnoc n q) (gput ws w h n v)))))
                (list newq weights)
                ns))
              )))))
      )
    (loop (list->queue '((0 . 0))) (alist->ff '((0 . 0))))))

(define (foreach f xs)
  (cond
   ((null? xs) '())
   (#t
    (f (car xs))
    (foreach f (cdr xs)))))

(define (walkback w h g c)
  (cond
   ((equal? c '(0 . 0)) '())
   (#t
    (let ((p (gget g w h c #false)))
      (cons c (walkback w h g p))))))

(define (main . args)
  (let* ((w 71) (h 71) (l 1024)
  ;; (let* ((w 7) (h 7) (l 12)
         (cs (take *coords* l))
         (res (solve w h l))
         (end (cons (- w 1) (- h 1)))
         (path (walkback w h res end)))
    (each
     (lambda (y)
       (each
        (lambda (x)
          (let ((c (member (cons x y) path))
                (w (member (cons x y) cs)))
            (display (if c "0" (if w "#" ".")))))
        (iota w))
       (newline))
     (iota h))
    (print (length path))
    (foreach ;; for some reason using "each" here only runs one iteration. map works fine but accumulates memory
     (lambda (il)
       (let* ((base (+ il 1024))
              (ires (solve w h (+ base 1))))
         (print il)
         (when (not (gget ires w h end #false))
           (display (string-append "broken " (number->string il) ": "))
           (print (list-ref *coords* base)))))
     (iota (length *coords*)))))

(fasl-save main "main.bl")
