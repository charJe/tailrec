(import 'tailrec:tailrec)
(import 'tailrec:deftailrec)

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

(deftailrec fib-key (n &key (a 0) (b 1))
  (cond
     ((= 0 n) a)
     ((= 1 n) b)
     (:else (fib (- n 1) b (+ a b)))))

(deftailrec fib-rest (n &rest vars)
  (let ((a (or (first vars) 0))
        (b (or (second vars) 1)))
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
           (fib-reg 10)
           (fib-key 10)
           (fib-rest 10)))

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

;; 0 warnings
(tailrec
 (defun stupid-filter (list
                       &optional acc)
   (let ((item (first list)))
     (cond
       ((null list) (reverse acc))
       ((evenp item)
        (stupid-filter (rest list)
                       (cons item acc)))
       (:else
        (let ((moder (mod item 3)))
          (cond
            ((= 0 moder)
             (stupid-filter (rest list)
                            (cons item acc)))
            ((= 1 moder)
             (stupid-filter (rest list)
                            (cons (+ item 1) acc)))
            (:else
             (stupid-filter (rest list)
                            acc)))))))))


(deftailrec floor-to-zero (number divisor)
  (if (= 0 divisor)
      (values number divisor)
      (multiple-value-call
          'floor-to-zero
        (floor number divisor))))

(assert (= 2 (length (multiple-value-list (floor-with-extra-steps 5 2)))))
