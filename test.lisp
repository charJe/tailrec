(import 'tailrec-llgpl:tailrec)

(defun fib-reg (n)
  "Return the Nth Fibonnaci number."
  (if (< n 2)
      n
      (+ (fib-reg (- n 1))
         (fib-reg (- n 2)))))

;; 0 warnings
(tailrec
 (defun fib (n &optional (a 0) (b 1))
   "Return the Nth Fibonnaci number."
   (cond
     ((= 0 n) a)
     ((= 1 n) b)
     (:else (fib (- n 1) b (+ a b))))))

;; this would be better to memoize
;; 2 warnings
(tailrec
 (defun fib-bad (n)
   "Return the Nth Fibonnaci number."
   (if (< n 2)
       n
       (+ (fib-bad (- n 1))
          (fib-bad (- n 2))))))

(assert (= (fib 10)
           (fib-bad 10)
           (fib-reg 10)))

(defun foo (n)
  (print n)
  (when (plusp n)
    (foo (1- n)))
  (print 'end))

;; 1 warning
(tailrec
 (defun foo-op (n)
   (print n)
   (when (plusp n)
     (foo-op (1- n)))
   (print 'end)))

(assert (string=
         (with-output-to-string (*standard-output*)
           (foo 4))
         (with-output-to-string (*standard-output*)
           (foo-op 4))))
