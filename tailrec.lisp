(defpackage tailrec
  (:use #:cl)
  (:export tailrec))
(in-package #:tailrec)

(defvar *at-tail* t)
(defvar *first-form* t)
(defvar *last-form*)
(defvar *non-tail-warning*)
(defvar *optimized*)

(defun conditionalp (symbol)
  (and (symbolp symbol)
       (member symbol '(if))))

(defun optimize-tails (name start args form
                       &aux (form (macroexpand form)))
  "Return FORM optimize to tail call NAME.
START is the symbol at the begininng of NAME.
ARGS is the symbol storing the arguments before a jump."
  (cond
    ((atom form) form)
    ((and (symbolp (first form))
          (eql name (first form)))
     (if (and *at-tail*
              (or (conditionalp *first-form*)
                  (eql form *last-form*)))
         (progn
           (setq *optimized* t)
           `(progn
            (setq ,args (list ,@(rest form)))
            (go ,start)))
         (progn
           (setq *at-tail* nil)
           (setq *non-tail-warning* t)
           form)))
    (:else
     (let* ((*first-form* (first form))
            (*last-form* (first (last form)))
            (*at-tail* (and (symbolp *first-form*)
                            (special-operator-p *first-form*))))
       (map 'list
            (lambda (form)
              (optimize-tails name start args form))
            form)))))

(defmacro tailrec (defunition)
  "Ensure that BODY is tail call optimized when calling def"
  (destructuring-bind
      (def name lambda-list &body body)
      defunition
    (let* ((start (gensym))
           (result (gensym))
           (args (gensym))
           (*non-tail-warning* nil)
           (*optimized* nil)
           (optimized
             (optimize-tails name start args body))
           (fun `(,name ,lambda-list
                        (let ((,result nil)
                              (,args nil))
                          (tagbody
                             ,start
                             (setq ,result
                                   (if ,args
                                       (destructuring-bind ,lambda-list ,args
                                         ,@optimized)
                                       (progn ,@optimized))))
                          ,result))))
      (when *non-tail-warning*
        (warn "~a is not a tail recursive" name))
      (cond
        ((not *optimized*) defunition)
        (def (cons def fun))
        (:else fun)))))

(defmacro nlet (name bindings &body body)
  `(labels (,(macroexpand
                 `(tailrec
                   (nil ,name
                    ,(map 'list
                      (lambda (binding)
                        (if (consp binding)
                            (first binding)
                            binding))
                      bindings)
                    ,@body))))
     (,name ,@(map 'list
                   (lambda (binding)
                     (when (consp binding)
                       (second binding)))
                   bindings))))

