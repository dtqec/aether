;;;; process/process.lisp
;;;;
;;;; A DSL for simulating a family of network-enabled processes that process
;;;; commands against a regular heartbeat.

(in-package #:aether)

;;;
;;; globals used to hold onto Lisp code.
;;;

(global-vars:define-global-var **dpu-macros-available** nil
  "Each entry is of the form (MACRO-NAME ARGUMENT-LIST DECLS . BODY).")

(global-vars:define-global-var **dpu-flets-available** nil
  "Each entry is of the form (FUNCTION-NAME ARGUMENT-LIST DECLS . BODY).")

;;;
;;; the PROCESS abstraction: a persistent bit of code that runs alongside other
;;; bits of code, perhaps in timeshare fashion and perhaps on separate hardware.
;;;

(defclass process ()
  ((process-courier
    :accessor process-courier
    :initform *local-courier*
    :initarg :process-courier
    :documentation "Holds a reference to the `COURIER' which services this `PROCESS'.")
   (process-key
    :accessor process-key
    :initform (register)
    :initarg :process-key
    :documentation "Holds the `PROCESS's `ADDRESS' on which it receives messages. (Use `PROCESS-PUBLIC-ADDRESS' to extract an `ADDRESS' to be used to send messages to this `PROCESS'.)")
   (process-clock-rate
    :accessor process-clock-rate
    :initarg :process-clock-rate
    :documentation "The number of times per unit of `SIMULATION' time that this `PROCESS' gets to act.")
   (process-command-stack
    :accessor process-command-stack
    :initform `((START))
    :documentation "A stack populated with the instructions yet to be executed by this PROCESS.  Modify this using `PROCESS-CONTINUATION'.")
   (process-data-stack
    :accessor process-data-stack
    :initform nil
    :documentation "A stack populated with data, typically \"data frames\" pushed and popped by sequences of related commands.")
   (process-debug?
    :accessor process-debug?
    :initform nil
    :initarg :debug?
    :documentation "A boolean flag. If T, debug messages are logged.")
   (process-exhaust-inbox?
    :accessor process-exhaust-inbox?
    :initarg :process-exhaust-inbox?
    :initform t
    :type boolean
    :documentation "Defaults to T, which means that the `PROCESS' will go through its entire message queue in a single clock tick, and then (in the same tick) take an action specified by the command stack. Otherwise, the `PROCESS' will immediately execute a command after handling either zero or one message(s).")
   (process-peruse-inbox?
    :accessor process-peruse-inbox?
    :initarg :process-peruse-inbox?
    :initform t
    :type boolean
    :documentation "Defaults to T, which means that the `PROCESS' will search through the whole inbox when trying to find a match for clause. Otherwise, the `PROCESS' will only look at the first message in the inbox. Essentially toggles between clause-order precedence and inbox-order predence.")
   (process-asleep-since
    :accessor process-asleep-since
    :initarg :process-asleep-since
    :initform nil
    :type (or null fraction)
    :documentation "FRACTION is the time at which the process was removed from the simulation heap and set for wake-on-network activity.  NIL is a process which is currently in the simulation heap."))
  (:documentation "Models an individual concurrently-executing process.  On each tick, a `PROCESS' first checks its public mailbox for messages matched by `DEFINE-MESSAGE-DISPATCH' and, if an appropriate message is found, that message is processed.  If no matching message is found, the first command in `PROCESS-COMMAND-STACK' is processed instead, according to the semantics defined by `DEFINE-PROCESS-UPKEEP'.

IMPORTANT NOTE: Use #'SPAWN-PROCESS to generate a new PROCESS object."))

(defmethod print-object ((object process) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~a" (address-channel (process-key object)))))

(defun process-public-address (process)
  "Extracts the public ADDRESS associated to PROCESS."
  (check-type process process)
  (public-address (process-key process)))

(defun process-continuation (process &rest commands)
  "Installs `COMMANDS' to be executed as next steps by `PROCESS'."
  (setf (process-command-stack process)
        (append commands (process-command-stack process))))

(defun mailbox-size (process)
  "Calculates the size of the malibox message queue for `PROCESS'."
  (with-slots (channel secret) (process-key process)
    (assert (eq secret (gethash channel (courier-secrets *local-courier*)))
            () "Cannot retrieve mailbox size -- mailbox secret is incorrect.")
    (q-len (gethash channel (courier-inboxes *local-courier*)))))

(defun clear-inbox (process)
  "Empties out the message queue in `PROCESS's inbox at `*LOCAL-COURIER*'"
  (with-slots (channel secret) (process-key process)
    (assert (eq secret (gethash channel (courier-secrets *local-courier*)))
            () "Cannot clear inbox -- mailbox secret is incorrect.")
    ;; reset the courier-inboxes entry to an empty queue
    (setf (gethash channel (courier-inboxes *local-courier*)) (make-q))))

(defgeneric process-upkeep (server command command-args)
  (:documentation "If a PROCESS has no pending requests to service, it may want to perform some computation of its own, which happens in this routine.  Such computations typically include sending messages and listening on private channels for responses.")
  (:method (server command command-args)
    (declare (ignore command-args))
    (error "Undefined command ~a for server of type ~a."
           command (type-of server))))

(defgeneric %message-dispatch (node)
  (:documentation "Use DEFINE-MESSAGE-DISPATCH to install methods here."))

(defun finish-handler ()
  "Return from the active aether process handler."
  (error () "Not available outside of an aether process handler."))

(defmacro define-message-handler (handler-name ((process process-type) (message message-type)) &body body)
  "Defines a function to be invoked by DEFINE-MESSAGE-DISPATCH."
  (multiple-value-bind (body decls documentation) (a:parse-body body :documentation t)
    `(defmethod ,handler-name ((,process ,process-type)
                               (,message ,message-type))
       ,@decls
       ,@(list documentation)
       (check-type ,message ,message-type)
       (check-type ,process ,process-type)
       (macrolet ((finish-handler () '(return-from ,handler-name)))
         (flet ((log-entry (&rest initargs)
                  (when (process-debug? ,process)
                    (apply #'log-entry
                           (append initargs
                                   (list :time (now)
                                         :source ,process))))))
           (flet ((send-message (destination payload)
                    (log-entry :entry-type ':send-message
                               :destination destination
                               :payload (copy-structure payload))
                    (send-message destination payload)))
             (declare (ignorable #'send-message))
             ,@body))))))

(define-message-handler handle-message-RTS
    ((process process) (message message-RTS))
  "Default handler for when a `PROCESS' receives a `MESSAGE-RTS'. Throws an error."
  (error "Got an RTS"))

(defmacro define-message-dispatch (node-type &body clauses)
  "Defines \"automatic\" message handlers associated to a particular subtype of `PROCESS'.  Each handler is specified by a tuple of form (MESSAGE-TYPE MESSAGE-HANDLER &OPTIONAL GUARD).  As with `RECEIVE-MESSAGE', each clause is processed in turn, according to the following rules:

  + If supplied, `GUARD' is evaluated with the `PROCESS' in question bound to the place `PROCESS-TYPE'.  If `GUARD' evaluates to NIL, proceed to the next clause.
  + Check the message queue at the public address for an item of type `MESSAGE-TYPE'.  If such a message is found, call the associated `MESSAGE-HANDLER' with lambda triple (PROCESS MESSAGE TIME).  Otherwise, proceed to the next clause.

There is one exception: (CALL-NEXT-METHOD) is also a legal clause, and it references to the message handling table installed via DEFINE-MESSAGE-DISPATCH on this process type's parent.

NOTES:
  + If no clause is matched, execution proceeds to the semantics specified by `DEFINE-PROCESS-UPKEEP'.
  + Automatically appends a `MESSAGE-RTS' clause which calls `HANDLE-MESSAGE-RTS' and results in an error. Because of this, we set `CATCH-RTS?' to NIL when processing clauses and building `RECEIVE-MESSAGE' blocks. Otherwise, it would be impossible to override the default handling of `MESSAGE-RTS'es.  Additionally, this extra handler is _not_ inherited through (CALL-NEXT-METHOD).
  + `PROCESS-PERUSE-INBOX?' is passed along to `RECEIVE-MESSAGE', where it determines how we search for a message to handle.

WARNING: These actions are to be thought of as \"interrupts\". Accordingly, you will probably stall the underlying `PROCESS' if you perform some waiting action here, like the analogue of a `SYNC-RECEIVE'."
  (a:with-gensyms (node message results trapped?)
    `(defmethod %message-dispatch ((,node ,node-type))
       ,@(mapcar
          (lambda (clause)
            (cond
              ((and (listp clause)
                    (= 1 (length clause))
                    (symbolp (first clause))
                    (string= "CALL-NEXT-METHOD" (symbol-name (first clause))))
               `(multiple-value-bind (,results ,trapped?) (call-next-method)
                  (when ,trapped?
                    (return-from %message-dispatch (values ,results ,trapped?)))))
              ((and (listp clause)
                    (member (length clause) '(2 3)))
               (destructuring-bind (message-type receiver . rest) clause
                 `(when (let ((,node-type ,node))
                          (declare (ignorable ,node-type))
                          ,(or (first rest) t))
                    (receive-message ((process-key ,node) ,message
                                      :catch-RTS? nil
                                      :peruse-inbox? (process-peruse-inbox?
                                                      ,node))
                      (,message-type
                       (when (process-debug? ,node)
                         (log-entry :time (now)
                                    :entry-type ':handler-invoked
                                    :source ,node
                                    :message-id (message-message-id ,message)
                                    :payload-type ',message-type))
                       (return-from %message-dispatch
                         (values
                          (funcall ,receiver ,node ,message)
                          t)))))))
              (t
               (error "Bad DEFINE-MESSAGE-DISPATCH clause: ~a" clause))))
          clauses))))

(defun message-dispatch (node)
  "Use DEFINE-MESSAGE-DISPATCH to install methods here."
  (multiple-value-bind (results trapped?) (%message-dispatch node)
    (declare (ignore results))
    (when trapped?
      (return-from message-dispatch t)))
  (receive-message ((process-key node) message
                    :catch-RTS? nil ; we do this ourselves so we can log-entry
                    :peruse-inbox? (process-peruse-inbox? node))
    (message-RTS
     (when (process-debug? node)
       (log-entry :time (now)
                  :entry-type ':handler-invoked
                  :source node
                  :message-id (message-message-id message)
                  :payload-type (type-of message)))
     (return-from message-dispatch
       (values (funcall 'handle-message-RTS node message) t)))))

;; TODO: DEFINE-DPU-MACRO and DEFINE-DPU-FLET don't check syntactic sanity at
;;       their runtime, they wait for DEFINE-PROCESS-UPKEEP to discover it.
(defmacro define-dpu-macro (macro-name argument-list &body body)
  "Defines a local macro to be used inside of DEFINE-PROCESS-UPKEEP.

NOTE: Automatically binds PROCESS-NAME to its value from within D-P-U."
  (a:with-gensyms (definition-pair)
    (let ((warning (format nil "~a is not available outside the body of DEFINE-PROCESS-UPKEEP."
                           macro-name)))
      (multiple-value-bind (subbody decls docstring) (a:parse-body body :documentation t)
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ;; define toplevel macro
           ,(unless (fboundp macro-name)
              `(defmacro ,macro-name ,argument-list
                 ,(if docstring
                      (format nil "~a~&~%NOTE: ~a" docstring warning)
                      (format nil "NOTE: ~a" warning))
                 (declare (ignorable ,@(macro-lambda-list-places argument-list)))
                 (error ,warning)))
           ;; store actual macro definition
           (let ((,definition-pair (assoc ',macro-name **dpu-macros-available**)))
             (unless ,definition-pair
               (setf ,definition-pair (cons ',macro-name nil))
               (push ,definition-pair **dpu-macros-available**))
             (setf (cdr ,definition-pair)
                   (list* ',argument-list
                          ',decls
                          ',subbody))))))))

(defmacro define-dpu-flet (function-name argument-list &body body)
  "Defines a local function to be used inside of DEFINE-PROCESS-UPKEEP.

NOTE: Automatically binds ACTIVE? and PROCESS-NAME to their values from within D-P-U."
  (a:with-gensyms (definition-pair)
    (let ((warning (format nil "~a is not available outside the body of DEFINE-PROCESS-UPKEEP."
                           function-name)))
      (multiple-value-bind (subbody decls docstring) (a:parse-body body :documentation t)
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ;; define toplevel function
           ,(unless (fboundp function-name)
              `(defun ,function-name ,argument-list
                 ,(if docstring
                      (format nil "~a~&~%NOTE: ~a" docstring warning)
                      (format nil "NOTE: ~a" warning))
                 (declare (ignorable ,@(macro-lambda-list-places argument-list)))
                 (error ,warning)))
           ;; store actual function definition
           (let ((,definition-pair (assoc ',function-name **dpu-flets-available**)))
             (unless ,definition-pair
               (setf ,definition-pair (cons ',function-name nil))
               (push ,definition-pair **dpu-flets-available**))
             (setf (cdr ,definition-pair)
                   (list* ',argument-list
                          ',decls
                          ',subbody))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun %dpu-flet-definitions (&key
                                  (active? (error "Supply ACTIVE?."))
                                  (process-name (error "Supply PROCESS-NAME.")))
    "This function produces a list of local function (re)definitions to be supplied in the body of DEFINE-PROCESS-UPKEEP.

NOTE: LOG-ENTRY is treated separately."
    (loop :for (name args decls . body) :in **dpu-flets-available**
          :collect `(,name ,args ,@decls
                           (let ((active? ,active?) (process-name ,process-name))
                             (prog1 (progn ,@body)
                               (setf ,active? active?
                                     ,process-name process-name))))))

  (defun %dpu-macrolet-definitions (&key
                                      (process-name (error "Supply PROCESS-NAME.")))
    "This function produces a list of local macro definitions to be supplied in the body of DEFINE-PROCESS-UPKEEP."
    (loop :for (name args decls . body) :in **dpu-macros-available**
          :collect `(,name ,args ,@decls
                           (let ((process-name ',process-name))
                             (declare (ignorable process-name))
                             ,@body)))))

(define-condition dpu-exit (condition) ()
  (:documentation "Used to terminate early whatever DEFINE-PROCESS-UPKEEP is currently running."))

(defmacro define-process-upkeep (((process-name process-type)) (command &rest command-args) &body body)
  "Defines the behavior of a particular PROCESS (of type PROCESS-TYPE) as it enacts a COMMAND.

PROCESS is COMMAND is a KEYWORD, and COMMAND-ARGS is a DESTRUCTURING-BIND-LAMBDA-LIST.

Locally enables the use of the function PROCESS-DIE and the special form SYNC-RECEIVE."
  (check-type command symbol)
  (multiple-value-bind (body decls docstring) (a:parse-body body :documentation t)
    (a:with-gensyms (command-place argument-list active?)
      (let ((macrolet-definitions (%dpu-macrolet-definitions :process-name process-name))
            (flet-definitions (%dpu-flet-definitions :active? active?
                                                     :process-name process-name)))
        `(defmethod process-upkeep ((,process-name ,process-type)
                                    (,command-place (eql ',command))
                                    ,argument-list)
           ,@(when docstring (list docstring))
           (declare (ignorable ,command-place ,process-name))
           (let ((*local-courier* (process-courier ,process-name)))
             ;; log-entry is treated separately, since we want the other FLETs
             ;; to pick it up and use it.
             (flet ((log-entry (&rest initargs)
                      ;; install automatic log entries
                      (when (process-debug? ,process-name)
                        (apply #'log-entry
                               (append initargs
                                       (list :source ,process-name
                                             :time (now)))))))
               (initialize-and-return ((,active? t))
                 (handler-case
                     (destructuring-bind ,command-args ,argument-list
                       ,@decls
                       (macrolet ,macrolet-definitions
                         ;; install log wrappers around common functions
                         (flet ,flet-definitions
                           (declare (ignorable ,@(loop :for (name . rest) :in flet-definitions
                                                       :collect `#',name)))
                           ;; announce start of upkeep
                           (log-entry :entry-type ':command
                                      :command ',command
                                      :arguments (copy-seq ,argument-list)
                                      :next-command (caar (process-command-stack ,process-name)))
                           ,@body)))
                   (dpu-exit ()  (values)))))))))))

(define-object-handler ((process process))
  "Determines the behavior of a generic PROCESS. See DEFINE-MESSAGE-DISPATCH and DEFINE-PROCESS-UPKEEP for details."
  (block nil
    (assert (null (process-asleep-since process)))
    (with-slots (process-key process-clock-rate process-exhaust-inbox?) process
      (let ((*local-courier* (process-courier process))
            (active? nil))
        (when (message-dispatch process)
          (when process-exhaust-inbox?
            (schedule process (now))
            (return)))
        (when (peek (process-command-stack process))
          (destructuring-bind (command &rest args) (pop (process-command-stack process))
            (setf active? (process-upkeep process command args))))
        (cond
          ;; tear down the process
          ((not active?)
           (unregister (process-key process)))
          ((process-asleep-since process)
           nil)
          (active?
           (schedule process (+ (now) (/ process-clock-rate)))))))))

(defmethod wake-up ((process process))
  (a:when-let ((since (process-asleep-since process)))
    (let ((next-tick (+ since
                        (/ (1+ (floor (- (now) since)
                                      (/ (process-clock-rate process))))
                           (process-clock-rate process)))))
      (assert (<= (now) next-tick))
      (setf (process-asleep-since process) nil)
      (schedule process next-tick))))
