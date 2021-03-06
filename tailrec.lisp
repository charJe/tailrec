(defpackage tailrec
  (:use #:cl)
  (:import-from #:trivial-with-current-source-form
                with-current-source-form)
  (:import-from #:trivial-macroexpand-all
                macroexpand-all)
  (:import-from #:alexandria
                plist-alist
                simple-program-error
                parse-ordinary-lambda-list
                parse-body)
  (:export deftailrec tailrec nlet))
(in-package #:tailrec)

(defvar *at-tail* t)
(defvar *first-form* t)
(defvar *last-form*)
(defvar *optimized*)

(defun conditionalp (symbol)
  (and (symbolp symbol)
       (member symbol '(if))))

(defun optimize-tails (name start args form)
  "Return FORM optimize to tail call NAME.
START is the symbol at the begininng of NAME.
ARGS is the symbol storing the arguments before a jump."
  (cond
    ((atom form) form)
    ((and (symbolp (first form))
          (eql name (first form)))
     (if (and *at-tail*
              (or (conditionalp *first-form*)
                  (equal form *last-form*)))
         (progn
           (setq *optimized* t)
           `(progn
            (setq ,args (list ,@(rest form)))
            (go ,start)))
         (with-current-source-form (form)
           (setq *at-tail* nil)
           (warn "~a is not a tail recursive" name)
           form)))
    (:else
     (let* ((first-form (first form))
            (*at-tail* (and *at-tail*
                            (symbolp first-form)
                            (special-operator-p first-form)
                            (or (equal form *last-form*)
                                (conditionalp *first-form*))))
            (*first-form* first-form)
            (*last-form* (first (last form))))
       (map 'list
            (lambda (form)
              (optimize-tails name start args form))
            form)))))

(defmacro tailrec (defunition)
  "Ensure that BODY is tail call optimized when calling def"
  (destructuring-bind
      (def name lambda-list &body body) defunition
    (multiple-value-bind
          (body declarations documentation)
        (parse-body body :documentation t)
      (multiple-value-bind
            (params optional-params rest-param key-params allow-other-keys-p aux keyp)
          (parse-ordinary-lambda-list lambda-list)
        (declare (ignore aux allow-other-keys-p))
      (let* ((start (gensym))
             (result (gensym))
             (args (gensym))
             (mkey-args (gensym))
             (*optimized* nil)
             (optional (map 'list 'first optional-params))
             (optional-values (map 'list 'second optional-params))
             (keys (map 'list 'cadar key-params))
             (key-keys (map 'list 'caar key-params))
             (key-values (map 'list 'second key-params))
             (body (map 'list 'macroexpand-all body))
             (*last-form* (first (last body)))
             (optimized
               (map 'list
                    (lambda (form)
                      (optimize-tails name start args form))
                    body))
             (fun
               `(,name
                 ,lambda-list
                 ,documentation
                 ,declarations
                 (let ((,result nil)
                       (,args nil))
                   (tagbody ,start
                      (setq ,result
                            (progn
                              (when ,args
                                ;; set required args
                                ,@(loop for param in params
                                        for i from 0 collect
                                        `(setq ,param (elt ,args ,i)))
                                ;; shift args
                                (setq ,args (nthcdr ,(length params) ,args))
                                ;; set optional parameters
                                ,(when optional-params
                                   `(progn
                                      ;; they might not all be supplied
                                      ;; so set default values
                                      ,@(loop for opt in optional
                                              for default in optional-values collect
                                              `(setq ,opt ,default))
                                      (let ((length (length ,args)))
                                        ,@(loop for opt in optional
                                                for i from 0 collect
                                                `(when (< ,i length)
                                                   (setq ,opt (elt ,args ,i)))))
                                      ;; shift args
                                      (setq ,args (nthcdr ,(length optional) ,args))))
                                ;; set rest
                                ,(when rest-param
                                   `(setq ,rest-param ,args))
                                ;; set keys
                                ,(when keyp
                                   `(let ((,mkey-args (plist-alist ,args)))
                                      ,@(loop for param in keys
                                              for key in key-keys
                                              for init in key-values collect
                                              `(setf ,param (or (cdr (assoc ,key ,mkey-args))
                                                                ,init))))))
                              (multiple-value-list (progn ,@optimized)))))
                   (values-list ,result)))))
        (cond
          ((not *optimized*) defunition)
          (def (cons def fun))
          (:else fun)))))))

(defmacro deftailrec (name lambda-list &body body)
  "Wrapper for `tailrec' to reduce nesting by one layer."
  `(tailrec (defun ,name ,lambda-list ,@body)))

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
