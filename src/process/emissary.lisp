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
  "Interrupt-based RPC handlers are expected to quickly return control to the main thread of execution, and any maneuvers which take nontrivial simulation time are modeled as commands pushed onto the underlying process's command stack.  However, this is executed serially with whatever the process was doing when it received the interrupt.  It is sometimes more convenient to process the tasks in parallel, which we model by delegating the new task to a newly spawned side-process.

This macro mimics DEFINE-MESSAGE-HANDLER while setting up this manner of parallel execution."
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
