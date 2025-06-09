;;;; cast.lisp
;;;;
;;;; Built-in broadcast and convergecast facilities for `PROCESS'es. See
;;;; http://www.cs.yale.edu/homes/aspnes/classes/465/notes.pdf for more
;;;; information.

(in-package #:aether)

(defun return-from-cast (&optional value reason)
  "Allows the user to return early from a broadcast or convergecast operation."
  (declare (ignore value reason))
  (error "RETURN-FROM-CAST only available in DEFINE-BROADCAST-HANDLER or DEFINE-CONVERGECAST-HANDLER."))

(defstruct (broadcast-frame)
  "Data frame for a `BROADCAST' operation. The `ABORTING?' flag is used to signal to a `BROADCAST' or `CONVERGECAST' command to terminate immediately. If the operation doesn't abort, the `MESSAGE' is broadcasted to `TARGETS'. If we are awaiting replies from the broadcast, we can set `HANDLE-RTS?' to T if we want to pass a `:RETURNED?' keyword argument to `WITH-REPLIES' (and thus handle RTSes gracefully)."
  (aborting?    nil :type boolean)
  (handle-rts?  nil :type boolean)
  (message      nil :type (or null message))
  (targets      nil :type list))

(defun push-broadcast-frame (&key aborting? handle-rts? message targets)
  "Convenience function for creating a `BROADCAST-FRAME' and pushing it onto the `PROCESS-DATA-STACK'."
  (declare (ignore aborting? handle-rts? message targets))
  (error "PUSH-BROADCAST-FRAME only available in DEFINE-BROADCAST-HANDLER."))

(defmacro define-broadcast-handler (handler-name
                                    ((process process-type) (message message-type))
                                    &body body)
  "This macro augments `DEFINE-MESSAGE-HANDLER', by pushing a `BROADCAST' command onto the `PROCESS's command stack. This command takes no arguments, but expects a `BROADCAST-FRAME' to be on the data stack when it is called. Thus, either the handler must prepare the data frame, or it must be prepared by whatever script is pushed onto the command stack by the handler. Additionally, inside the context of `DEFINE-BROADCAST-HANDLER' we have access to a pair of helper functions:

1. `PUSH-BROADCAST-FRAME', which creates a `BROADCAST-FRAME' and pushes it onto the data stack. If no `MESSAGE' is provided to the function, it defaults to the `MESSAGE' currently being handled. If a user calls `PUSH-BROADCAST-FRAME' twice in the same handler, an error will be raised.
2. `RETURN-FROM-CAST', which allows the user to terminate the broadcast operation early by sending up an acknowledgement (optionally specifying its contents) to the original sender of `MESSAGE'.

WARNING: `RETURN-FROM-CAST' calls `PUSH-BROADCAST-FRAME' as part of the aborting process. If a frame has already been pushed onto the data stack, we instead alter that frame rather than pushing an additional one (which could have strange consequences). Additionally, it is important to note that `RETURN-FROM-CAST' uses `FINISH-WITH-SCHEDULING' in order to return from the handler early."
  (a:with-gensyms (broadcast-frame reply-channel)
    `(define-message-handler ,handler-name
         ((,process ,process-type) (,message ,message-type))
       (let ((,broadcast-frame nil)
             (,message (copy-message message))
             (,reply-channel (message-reply-channel ,message)))
         (labels ((push-broadcast-frame (&key aborting?
                                              handle-rts?
                                              (message ,message)
                                              targets)
                    ;; We would like to prevent the user from pushing more than
                    ;; one `BROADCAST-FRAME' onto the `PROCESS-DATA-STACK'.
                    (assert (null ,broadcast-frame) ()
                            "Cannot push two BROADCAST-FRAMEs from one handler.")
                    (setf ,broadcast-frame
                          (make-broadcast-frame :aborting? aborting?
                                                :handle-rts? handle-rts?
                                                :message message
                                                :targets targets))
                    (push ,broadcast-frame (process-data-stack ,process)))
                  (return-from-cast (&optional value (reason ':none-provided))
                    (log-entry :entry-type ':terminating-broadcast
                               :reason reason)
                    ;; Push a frame with `:ABORTING?' set to T, so that the
                    ;; pre-pushed `BROADCAST' command is ignored. If we have
                    ;; already pushed one, just set its `ABORTING?' flag.
                    (if ,broadcast-frame
                        (setf (broadcast-frame-aborting? ,broadcast-frame) t)
                        (push-broadcast-frame :aborting? t))
                    (when ,reply-channel
                      (send-message ,reply-channel (make-message-rpc-done
                                                    :result value)))
                    (finish-handler)))
           (declare (ignorable #'push-broadcast-frame #'return-from-cast))
           ;; Push `BROADCAST' before executing the body, in case the body
           ;; pushes an additional script onto the command stack that is
           ;; meant to be executed before continuing the broadcast.
           (process-continuation ,process `(BROADCAST))
           ,@body)))))

(define-process-upkeep ((process process))
    (BROADCAST)
  "Pops a frame off of the `PROCESS's data stack, and after checking that it is in fact a `BROADCAST-FRAME', unpacks `ABORTING?', `HANDLE-RTS?', `MESSAGE' and `TARGETS' from the frame. Unless `ABORTING?' is T, forwards along a copy of `MESSAGE' to all `TARGETS' and optionally awaits a reply and sends back an acknowledgement back to the `MESSAGE's `REPLY-CHANNEL' if the reply channel is not null. While awaiting replies, we handle RTSes gracefully if `HANDLE-RTS?' is T."
  (let ((frame (pop (process-data-stack process))))
    (check-type frame broadcast-frame)
    (with-slots (aborting? handle-rts? message targets) frame
      (with-slots (reply-channel) message
        (unless aborting?
          (cond
            ;; If `TARGETS' is NIL, just optionally send back acknowledgement.
            ((endp targets)
             (when reply-channel
               (send-message reply-channel (make-message-rpc-done))))
            (t
             (log-entry :entry-type ':broadcasting
                        :handle-rts? handle-rts?
                        :message message
                        :targets targets)
             (flet ((payload-constructor ()
                      (copy-message message)))
               (cond
                 ((and handle-rts? reply-channel)
                  (with-replies (replies :returned? returned?)
                                (send-message-batch #'payload-constructor targets)
                    (send-message reply-channel (make-message-rpc-done))))
                 (reply-channel
                  (with-replies (replies)
                                (send-message-batch #'payload-constructor targets)
                    (send-message reply-channel (make-message-rpc-done))))
                 (t
                  ;; No need to await acknowledgments when no `REPLY-CHANNEL'.
                  (send-message-batch #'payload-constructor targets)))))))))))

(defstruct (convergecast-frame (:include broadcast-frame))
  "Data frame for a `CONVERGECAST' operation. The `INPUT' is the value calculated at the current level of recursion, to be passed to `FUNC' along with the `REPLIES' from the subsequent level of recursion. The value V passed up is determined as V = (FUNCALL FUNC INPUT REPLIES), and thus the definition of `FUNC' should be something like:

DEFUN FUNC INPUT REPLIES

Where `REPLIES' is assumed to be a `LIST'. Additionally, when `HANDLE-RTS?' is true, `FUNC' should be constructed to handle NIL values in `INPUT' or `REPLIES' gracefully. Inherits from `BROADCAST-FRAME'."
  (func nil :type (or null function))
  (input))

(defun push-convergecast-frame (&key
                                  aborting?
                                  handle-rts?
                                  func
                                  input
                                  message
                                  targets)
  "Convenience function for creating a `CONVERGECAST-FRAME' and pushing it onto the `PROCESS-DATA-STACK'."
  (declare (ignore aborting? handle-rts? func input message targets))
  (error "PUSH-CONVERGECAST-FRAME only available in DEFINE-CONVERGECAST-HANDLER."))

(defun %convergecast-body (process message body)
  (a:with-gensyms (convergecast-frame reply-channel)
    `((let ((,convergecast-frame nil)
            (,message (copy-message ,message))
            (,reply-channel (message-reply-channel ,message)))
        (labels ((push-convergecast-frame (&key aborting?
                                                handle-rts?
                                                func
                                                input
                                                (message ,message)
                                                targets)
                   (assert (null ,convergecast-frame) ()
                           "Cannot push two CONVERGECAST-FRAMEs from one handler.")
                   (setf ,convergecast-frame
                         (make-convergecast-frame :aborting? aborting?
                                                  :handle-rts? handle-rts?
                                                  :func func
                                                  :input input
                                                  :message message
                                                  :targets targets))
                   (push ,convergecast-frame
                         (process-data-stack ,process)))
                 (return-from-cast (&optional value (reason ':none-provided))
                   (log-entry :entry-type ':terminating-convergecast
                              :reason reason)
                   ;; Push a frame with `:ABORTING?' set to T, so that the
                   ;; pre-pushed `CONVERGECAST' command is ignored. If we have
                   ;; already pushed one, just set its `ABORTING?' flag.
                   (if ,convergecast-frame
                       (setf (convergecast-frame-aborting? ,convergecast-frame) t)
                       (push-convergecast-frame :aborting? t))
                   (when ,reply-channel
                     (send-message ,reply-channel (make-message-rpc-done
                                                   :result value)))
                   (finish-handler)))
          (declare (ignorable #'push-convergecast-frame #'return-from-cast))
          (process-continuation ,process `(CONVERGECAST))
          ,@body)))))

(defmacro define-convergecast-handler (handler-name
                                       ((process process-type)
                                        (message message-type))
                                       &body body)
  "This macro augments `DEFINE-MESSAGE-HANDLER', by pushing a `CONVERGECAST' command onto the `PROCESS's command stack. This command takes no arguments, but expects a `CONVERGECAST-FRAME' to be on the data stack when it is called. Thus, either the handler must prepare the data frame, or it must be prepared by whatever script is pushed onto the command stack by the handler. Additionally, inside the context of `DEFINE-CONVERGECAST-HANDLER' we have access to a pair of helper functions:

1. `PUSH-CONVERGECAST-FRAME', which creates a `CONVERGECAST-FRAME' and pushes it onto the data stack. If no `MESSAGE' is provided to the function, it defaults to the `MESSAGE' currently being handled. If a user calls `PUSH-BROADCAST-FRAME' twice in the same handler, an error will be raised.
2. `RETURN-FROM-CAST', which allows the user to terminate the convergecast operation early by sending up an acknowledgement (optionally specifying its contents) to the original sender of `MESSAGE'. It is recommended that a value is provided when returning from a convergecast, as it will be passed to a function (the function provided to the `CONVERGECAST-FRAME') when received by the original sender.

WARNING: `RETURN-FROM-CAST' calls `PUSH-CONVERGECAST-FRAME' as part of the aborting process. If a frame has already been pushed onto the data stack, we instead alter that frame rather than pushing an additional one (which could have strange consequences). Additionally, it is important to note that `RETURN-FROM-CAST' uses `FINISH-WITH-SCHEDULING' in order to return from the handler early."
  `(define-message-handler ,handler-name
       ((,process ,process-type) (,message ,message-type))
     ,@(%convergecast-body process message body)))

;; cf. define-message-subordinate
(defmacro define-convergecast-subordinate
    (handler-name
     ((process process-type)
      (message message-type))
     &body body)
  "Interrupt-based RPC handlers are expected to quickly return control to the main thread of execution, and any maneuvers which take nontrivial simulation time are modeled as commands pushed onto the underlying process's command stack.  However, this is executed serially with whatever the process was doing when it received the interrupt.  It is sometimes more convenient to process the tasks in parallel, which we model by delegating the new task to a newly spawned side-process.

This macro mimics DEFINE-CONVERGECAST-HANDLER while setting up this manner of parallel execution."
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
         ,@(%convergecast-body subprocess message body)))))

(define-process-upkeep ((process process))
    (CONVERGECAST)
  "Pops a frame off of the `PROCESS's data stack, and after checking that it is in fact a `CONVERGECAST-FRAME', unpacks `ABORTING?', `FUNC', `INPUT', `MESSAGE', and `TARGETS' from the frame. Unless `ABORTING?' is T, forwards along a copy of `MESSAGE' to all `TARGETS' and awaits replies. While awaiting replies, we handle RTSes gracefully if `HANDLE-RTS?' is T. Then, calculates a value to return by feeding the `INPUT' and all `REPLIES' to `FUNC' as (FUNCALL FUNC INPUT REPLIES). Finally, sends the computed result back to `REPLY-CHANNEL' if it is not null."
  (let ((frame (pop (process-data-stack process))))
    (check-type frame convergecast-frame)
    (with-slots (aborting? func input handle-rts? message targets) frame
      (unless aborting?
        (with-slots (reply-channel) message
          (cond
            ;; If `TARGETS' is NIL, just optionally send back the `INPUT'.
            ((endp targets)
             (when reply-channel
               (send-message reply-channel
                             (make-message-rpc-done :result input))))
            (t
             (log-entry :entry-type ':convergecasting
                        :func func
                        :input input
                        :message message
                        :targets targets)
             (flet ((payload-constructor ()
                      (copy-message message)))
               ;; We always await replies, even if there is no `REPLY-CHANNEL'.
               ;; This behavior deviates from that of `BROADCAST', as there is
               ;; no `CONVERGECAST' without some amount of computing & replying.
               ;; Thus, providing no `REPLY-CHANNEL' when convergecasting only
               ;; affects the originator.
               (cond
                 (handle-rts?
                  (with-replies (replies :returned? returned?)
                                (send-message-batch #'payload-constructor targets)
                    (let ((result (funcall func input replies)))
                      (when reply-channel
                        (send-message reply-channel
                                      (make-message-rpc-done :result result))))))
                 (t
                  (with-replies (replies)
                                (send-message-batch #'payload-constructor targets)
                    (let ((result (funcall func input replies)))
                      (when reply-channel
                        (send-message reply-channel
                                      (make-message-rpc-done :result result)))))))))))))))

;;;
;;; some `FUNC' examples
;;;

(defun reduce+ (input replies)
  "Sum the `INPUT' and all the `REPLIES'."
  (reduce #'+ (list* input replies)))

(defun reduce-max (input replies)
  "Find the `MAX' of the `INPUT' and all the `REPLIES'."
  (reduce #'max (list* input replies)))
