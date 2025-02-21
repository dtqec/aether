;;;; process/dpu-helpers.lisp
;;;;
;;;; Defines helper functions, macros, and commands to be made available within
;;;; the body of DEFINE-PROCESS-UPKEEP.

(in-package #:aether)

;;;
;;; flets
;;;

(define-dpu-flet process-die ()
  "PROCESS-DIE causes the current PROCESS to release its public mailbox and exit the parent SIMULATION loop."
  (setf active? nil))

(define-dpu-flet register ()
  "Adds logging functionality to REGISTER."
  (initialize-and-return ((channel (register)))
    (log-entry :entry-type 'register
               :new-channel (public-address channel))))

(define-dpu-flet send-message (destination payload)
  "Adds logging functionality to SEND-MESSAGE."
  (initialize-and-return ((result (send-message destination payload)))
    (when (process-debug? process-name)
      (setf (process-debug? process-name)
            (list ':time now
                  ':origin (type-of process-name)
                  ':destination-count 1
                  ':message (type-of payload))))
    (log-entry :entry-type 'send-message
               :destination destination
               :payload (copy-structure payload))))

(define-dpu-flet send-message-batch (payload-constructor
                                     destinations
                                     &key (replies? t))
  "Adds logging functionality to SEND-MESSAGE-BATCH."
  (initialize-and-return ((reply-addresses
                           (send-message-batch payload-constructor
                                               destinations
                                               :replies? replies?)))
    (when (process-debug? process-name)
      (setf (process-debug? process-name)
            (list ':time now
                  ':origin (type-of process-name)
                  ':destination-count (length destinations)
                  ':message (type-of (funcall payload-constructor)))))
    (log-entry :entry-type 'send-message-batch
               :destinations destinations
               :payload-constructor payload-constructor
               :replies? replies?)))

;;;
;;; macros
;;;

(define-dpu-macro %install-repeat-until
    ((&body repeater-body)
     (&body finalizer-body))
  "Irreversibly transfers control from the current command by replacing it with a `:REPEAT-UNTIL' command with the indicated `REPEATER' and `FINALIZER'."
  (a:with-gensyms (repeater finalizer)
    `(flet ((,repeater (,now)
              (declare (ignorable ,now))
              ,@repeater-body)
            (,finalizer (,now)
              (declare (ignorable ,now))
              ,@finalizer-body))
       (push (list 'REPEAT-UNTIL #',repeater #',finalizer)
             (process-command-stack ,process-name)))))

;; TODO: "SYNC"-RECEIVE is a somewhat misleading name.
;;       it's more like a busywaiting callback?
(define-dpu-macro sync-receive ((sync-channel sync-message-place) &body sync-clauses)
  "Models a blocking `RECEIVE-MESSAGE' command.

IMPORTANT WARNING: `SYNC-RECEIVE' returns after it finishes executing its body.  Any code following a `SYNC-RECEIVE' **will not** be executed.

NOTE: `MESSAGE-RTS' replies must be explicitly handled.  Otherwise, the default behavior is to throw an error, which can be seen in the definition of `RECEIVE-MESSAGE'."
  (a:with-gensyms (retval sr-done? record)
    `(%install-repeat-until
         ((log-entry :entry-type 'command
                     :time ,now
                     :command 'sync-receive
                     :next-command (caar (process-command-stack ,process-name))
                     :sync-channel ,sync-channel)
           (multiple-value-bind (,retval ,sr-done?)
               (receive-message (,sync-channel ,sync-message-place)
                 ,@(loop :for (clause-head . clause-body) :in sync-clauses
                         :collect `(,clause-head
                                    (a:when-let ((,record (process-debug? ,process-name)))
                                      (setf ,record (list* ':delta (- ,now (getf ,record ':time))
                                                           ,record))
                                      (remf ,record ':time)
                                      (apply #'tracer-store ,record))
                                    ,@clause-body)))
             (declare (ignore ,retval))
             ,sr-done?))
         (
          ;; no finalization after finishing the receive
          ))))

;; TODO: the pattern "block until some handle in a list is free" goes by the
;;       name "select" to POSIX system programmers.  If possible, one might
;;       factor that functionality out.  I _think_ it is accomplishable with
;;       %INSTALL-REPEAT-UNTIL and FLET.
(define-dpu-macro with-replies ((replies
                                 &key
                                 (returned? (gensym) returned?-p)
                                 (close? 't)
                                 (message-type 'message-rpc-done)
                                 (message-unpacker 'message-rpc-done-result))
                                addresses
                                &body body)
  "Amasses results from `MESSAGE-RPC-DONE' messages received on `ADDRESSES' into a list stored in `REPLIES'.  This list is sorted in the same order as `ADDRESSES'.  Execution of `BODY' resumes when each address has received such a message.

If `RETURNED?' is supplied, then in the event of a `MESSAGE-RTS', this flag is set.  If it is not supplied, this raises an error.

Typical use looks like:

 (with-replies
     (replies :returned? returned?)
     (send-message-batch #'make-blossom-msg-scan targets)
   (let ((reply (reduce #'unify-pongs replies)))
     (send-message reply-channel reply)))"
  (a:with-gensyms (listeners listener message remainder new-remainder reply record)
    `(let* ((,listeners ,addresses)
            (,replies (make-list (length ,listeners)))
            (,returned? nil)
            (,remainder
              (loop :for ,listener :in ,listeners
                    :for ,reply :on ,replies :by #'cdr
                    :collect (cons ,listener ,reply))))
       (declare (ignorable ,replies ,returned?))
       (%install-repeat-until
           (;; repeater
            (loop :for (,listener . ,reply) :in ,remainder
                  :when (receive-message (,listener ,message)
                          ,@(when returned?-p
                              `((message-RTS
                                 (when ,close?
                                   (unregister ,listener))
                                 (setf ,returned? t)
                                 nil)))
                          (,message-type
                           (when ,close?
                             (unregister ,listener))
                           (setf (car ,reply) (,message-unpacker ,message))
                           nil)
                          (otherwise
                           T))
                    :collect (cons ,listener ,reply) :into ,new-remainder
                  :finally (setf ,remainder ,new-remainder))
            (endp ,remainder))
           (;; finalizer
            (a:when-let ((,record (process-debug? ,process-name)))
              (setf ,record (list* ':delta (- ,now (getf ,record ':time)) ,record))
              (remf ,record ':time)
              (apply #'tracer-store ,record))
            ,@body)))))

;;;
;;; commands
;;;

(define-process-upkeep ((process T) now) (REPEAT-UNTIL callback finalize)
  "Repeatedly calls CALLBACK: (TIME) --> DONE? until DONE? is non-NIL.  Then, calls FINALIZE: (TIME) --> (EVENTS) once."
  (let ((done? (funcall callback now)))
    (cond
      ((and done? finalize)
       (funcall finalize now))
      (t
       (push `(REPEAT-UNTIL ,callback ,finalize) (process-command-stack process))))))
