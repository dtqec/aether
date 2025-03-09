;;;; process/emissary.lisp
;;;;
;;;; Helpers for defining nonblocking message handlers.

(in-package #:aether)

(defclass process-message-emissary (process)
  ()
  (:documentation "Dummy PROCESS used to host nonblocking message handlers."))

(define-message-dispatch process-message-emissary
  ;; no replies
  )

;; TODO: consider rolling this together with DEFINE-MESSAGE-HANDLER.  is the
;;       gain in emulation efficiency really worth it?  is there any potential
;;       for nasty side effects?
;; TODO: unclear that this plays well with message tracing.
(defmacro define-message-subordinate
    (handler-name
     ((process process-type) (message message-type))
     &body body)
  "Interrupt-based RPC handlers are expected to emit a reply to the caller.  This macro augments DEFINE-MESSAGE-HANDLER to reply to the caller with the last evaluated form."
  (a:with-gensyms (command servicer subprocess)
    `(progn
       (define-message-handler ,handler-name
           ((,process ,process-type) (,message ,message-type))
         (let ((,servicer (spawn-process 'process-message-emissary)))
           (schedule ,servicer (now))
           (setf (process-command-stack ,servicer) (list (list ',command ,process ,message))
                 (process-clock-rate ,servicer)    (process-clock-rate ,process)
                 (process-debug? ,servicer)        (process-debug? ,process))
           (values)))
       
       (define-process-upkeep ((,subprocess process-message-emissary))
           (,command ,process ,message)
         ,@body))))
