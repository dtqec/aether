;;;; utilities.lisp
;;;;
;;;; Common, package-wide utilities.

(in-package #:aether)

;; TODO: could handle shadowed names w/ some cleverness
;; TODO: deal with limited VALUES space by trimming the list at the macro level
(defmacro initialize-and-return ((&rest bindings) &body body)
  "Returns (in reverse order, as VALUES) the contents of BINDINGS after the evaluation of BODY."
  `(let* ,bindings
     ,@body
     (values ,@(nreverse (mapcar (lambda (x)
                                   (etypecase x
                                     (symbol x)
                                     (cons (car x))))
                                 bindings)))))

;; TODO: define a (setf ,value) local function
(defmacro dohash (((key value) hash-table &optional return-form) &body body)
  "Iterates over HASH-TABLE, defining a lexical binding KEY and a place VALUE for each iteration of BODY. Optionally evaluates RETURN-FORM at termination and returns its result; otherwise returns NIL.

WARNING: This routine is based on MAPHASH, which has undefined behavior if the structure of HASH-TABLE is modified during iteration (e.g., the addition of new entries, or the modification of any entry not currently being processed)."
  `(progn
     (maphash (lambda (,key ,value)
                ,@body)
              ,hash-table)
     ,return-form))

(defmacro destructuring-places (lambda-list expression &body body)
  "A variant of DESTRUCTURING-BIND that provides SETFs in the style of WITH-SLOTS, but it can only handle the required part of a DESTRUCTURING-LAMBDA-LIST."
  (check-type lambda-list cons)
  (a:with-gensyms (expr)
    (labels ((forbid-ampersands (symbol)
               (assert (not (member symbol '(&key &rest &allow-other-keys &aux &optional)))))
             (generate-bindings (lambda-list machine-expr human-expr)
               (etypecase lambda-list
                 (cons
                  (let ((new-symbol (gensym)))
                    (append (list `(CONS (,human-expr ,new-symbol ,machine-expr)))
                            (generate-bindings (car lambda-list)
                                               `(car ,new-symbol)
                                               `(car ,human-expr))
                            (generate-bindings (cdr lambda-list)
                                               `(cdr ,new-symbol)
                                               `(cdr ,human-expr)))))
                 (null
                  (list `(NULL (,human-expr))))
                 (symbol
                  (forbid-ampersands lambda-list)
                  (list `(PLACE (,lambda-list ,machine-expr)))))))
      (let* ((bindings (generate-bindings lambda-list expr expression))
             (macrolet-bindings
               (loop :for (binding-type binding-data) :in bindings
                     :when (eql 'PLACE binding-type)
                       :collect `(,(first binding-data) ,(second binding-data))))
             (let-bindings
               (loop :for (binding-type binding-data) :in bindings
                     :when (eql 'CONS binding-type)
                       :collect `(,(second binding-data) ,(third binding-data))))
             (assertions
               (policy-cond:policy-cond
                 ((< 0 safety)
                  (loop :for (binding-type binding-data) :in bindings
                        :when (or (eql 'NULL binding-type) (eql 'CONS binding-type))
                          :collect `(assert (typep ,(first binding-data) ',binding-type) ()
                                            "Expected ~a to be ~a, but got ~a"
                                            ',(first binding-data)
                                            ',binding-type
                                            ,(first binding-data))))
                 (t NIL))))
        (setf body `(let ((,expr ,expression))
                      ,@assertions
                      (let* ,let-bindings
                        (declare (ignorable ,@(mapcar #'first let-bindings)))
                        (symbol-macrolet ,macrolet-bindings
                          ,@body))))))
    body))

(defmacro ignorant-lambda (&body body)
  "Defines an anonymous function that discards all of its arguments."
  (a:with-gensyms (rest)
    `(lambda (&rest ,rest)
       (declare (ignore ,rest))
       ,@body)))

(defun peek (list)
  "Synonym for the PEEK operation on a stack."
  (check-type list list)
  (cond
    ((endp list)
     (values nil nil))
    (t
     (values (first list) t))))

(defmacro hash-let ((binding key table default-form) &body body)
  "Like GETHASH, except modifies the table."
  (a:with-gensyms (exists? key-holder table-holder)
    `(let ((,key-holder ,key)
           (,table-holder ,table))
       (multiple-value-bind (,binding ,exists?) (gethash ,key-holder ,table-holder)
         (unless ,exists?
           (setf ,binding ,default-form
                 (gethash ,key-holder ,table-holder) ,binding))
         ,@body))))

(defmacro assoc-default (item alist-place default-form &rest kwargs)
  (a:with-gensyms (pair item-place)
    `(let* ((,item-place ,item)
            (,pair (assoc ,item-place ,alist-place ,@kwargs)))
       (unless ,pair
         (setf ,pair (cons ,item-place ,default-form))
         (push ,pair ,alist-place))
       ,pair)))

(defun macro-lambda-list-places (lambda-list)
  "Collect the places that a macro lambda-list might bind."
  (labels ((parse-option (option)
             (cond
               ((typep option 'symbol)
                (list option))
               ((and (typep option 'list)
                     (= 2 (length option)))
                (list (first option)))
               ((and (typep option 'list)
                     (= 3 (length option)))
                (list (first option) (third option)))
               (t
                (error "Unknown optional lambda list chunk: ~a~%" option)))))
    (loop :with option? := nil
          :for rest :on lambda-list :by #'cdr
          :for item := (first rest)
          :unless (listp (cdr rest))
            :nconc (list (cdr rest))
          :nconc (cond
                   ((member item '(&key &optional))
                    (setf option? t)
                    nil)
                   ((member item '(&key &body &aux &allow-other-keys &optional &rest))
                    nil)
                   ((typep item 'symbol)
                    (list item))
                   ((and (not option?)
                         (typep item 'cons))
                    (macro-lambda-list-places item))
                   ((and option?
                         (typep item 'cons))
                    (parse-option item))
                   (t
                    (error "Unknown lambda list chunk: ~a~%" item))))))

;; see rigetti/quilc/src/utilities.lisp
(defmacro define-global-counter (counter-name incf-name)
  `(progn
     (declaim (type fixnum ,counter-name))
     (global-vars:define-global-var ,counter-name 0)
     (declaim (inline ,incf-name))
     (defun ,incf-name ()
       #+sbcl
       (sb-ext:atomic-incf ,counter-name)
       #+lispworks
       (system:atomic-incf ,counter-name)
       #+ecl
       (mp:atomic-incf ,counter-name)
       #+ccl
       (ccl::atomic-incf ,counter-name)
       #-(or ccl ecl sbcl lispworks)
       (incf ,counter-name))))
