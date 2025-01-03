(defparameter *inp* (mapcar #'parse-integer (uiop:read-file-lines "input.txt")))
(defun mix (s x) (logxor s x))
(defun prune (s) (logand s #xffffff))
(defun advance1 (s0)
  (let* ((s1 (prune (mix s0 (ash s0 6))))
         (s2 (prune (mix s1 (ash s1 -5)))))
    (prune (mix s2 (ash s2 11)))))
(defun advance (n s0)
  (loop
    :for s = s0 :then (advance1 s)
    :for i from 0 :to (- n 1)
    :finally (return s)))
(defparameter *p1* (reduce #'+ (mapcar (lambda (x) (advance 2000 x)) *inp*)))
(defun iota (m) (loop for n below m collect n))
(defun ht-keys (ht) (let ((keys nil)) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) ht) keys))
(defun advance-log-helper (log p0 p1 p2 p3 n s0)
  (let ((p4 (mod s0 10)))
    (when (and p0 p1 p2 p3)
      (let ((k (list (- p1 p0) (- p2 p1) (- p3 p2) (- p4 p3))))
        (unless (gethash k log nil)
          (setf (gethash k log) p4))))
    (if (= n 0) s0 (advance-log-helper log p1 p2 p3 p4 (- n 1) (advance1 s0)))))
(defun advance-log (n s0)
  (let ((log (make-hash-table :test #'equal)))
    (advance-log-helper log nil nil nil nil n s0)
    log))
(defparameter *logs* (mapcar (lambda (x) (advance-log 2000 x)) *inp*))
(defparameter *all-patterns* (make-hash-table :test #'equal))
(dolist (l *logs*) (maphash (lambda (k v) (setf (gethash k *all-patterns*) v)) l))
(defparameter *sums* (make-hash-table :test #'equal))
(maphash
 (lambda (k _)
   (declare (ignore v))
   (setf (gethash k *sums*) (reduce #'+ (mapcar (lambda (l) (gethash k l 0)) *logs*))))
 *all-patterns*)
(defparameter *max* (cons nil 0))
(maphash
 (lambda (k sum)
   (when (> sum (cdr *max*))
     (setf *max* (cons k sum))))
 *sums*)
(print *max*)
