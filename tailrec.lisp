(defpackage tailrec
  (:use #:cl)
  (:export tailrec nlet))
(in-package #:tailrec)

(defvar *at-tail* t)
(defvar *first-form* t)
(defvar *last-form*)
(defvar *optimized*)

(defun conditionalp (symbol)
  (and (symbolp symbol)
       (member symbol '(if))))

(defun non-expandable-p (form)
  (and (consp form)
       (symbolp (first form))
       (member (first form)
          '(declare declaim function lambda quote))))

(defun optimize-tails (name start args form
                       &aux (eform (macroexpand form)))
  "Return FORM optimize to tail call NAME.
START is the symbol at the begininng of NAME.
ARGS is the symbol storing the arguments before a jump."
  (cond
    ((non-expandable-p form) form)
    ((atom eform) eform)
    ((and (symbolp (first eform))
          (eql name (first eform)))
     (if (and *at-tail*
              (or (conditionalp *first-form*)
                  (eql eform *last-form*)))
         (progn
           (setq *optimized* t)
           `(progn
            (setq ,args (list ,@(rest eform)))
            (go ,start)))
         (progn
           (setq *at-tail* nil)
           (warn "~a is not a tail recursive" name)
           eform)))
    (:else
     (let* ((*first-form* (first eform))
            (*last-form* (first (last eform)))
            (*at-tail* (and (symbolp *first-form*)
                            (special-operator-p *first-form*))))
       (map 'list
            (lambda (eform)
              (optimize-tails name start args eform))
            eform)))))

(defmacro tailrec (defunition)
  "Ensure that BODY is tail call optimized when calling def"
  (destructuring-bind
      (def name lambda-list &body body)
      defunition
    (let* ((start (gensym))
           (result (gensym))
           (args (gensym))
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
