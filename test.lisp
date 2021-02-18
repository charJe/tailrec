(defun fib-reg (n)
  "Return the Nth Fibonnaci number."
  (if (< n 2)
      n
      (+ (fib-reg (- n 1))
         (fib-reg (- n 2)))))

(tailrec
 (defun fib (n &optional (a 0) (b 1))
   "Return the Nth Fibonnaci number."
   (cond
     ((= 0 n) a)
     ((= 1 n) b)
     (:else (fib (- n 1) b (+ a b))))))

;; this would be better to memoize
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
