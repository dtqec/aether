;;;; rpc.lisp
;;;;
;;;; This is a thin wrapper over the bare messaging framework in support of
;;;; common RPC idioms.  It is _not_ comprehensive.

(in-package #:aether)

;; TODO: this traps RETURN-FROM, but not FINISH-WITH-SCHEDULING.
(defmacro define-rpc-handler (handler-name
                              ((process process-type) (message message-type))
                              &body body)
  "Interrupt-based RPC handlers are expected to emit a reply to the caller.  This macro augments DEFINE-MESSAGE-HANDLER to reply to the caller with the last evaluated form."
  (a:with-gensyms (return-value reply-channel)
    `(define-message-handler ,handler-name
         ((,process ,process-type) (,message ,message-type))
       (let (,return-value)
         (setf ,return-value (block ,handler-name ,@body))
         (a:when-let ((,reply-channel (message-reply-channel ,message)))
           (send-message ,reply-channel (make-message-rpc-done :result ,return-value)))))))

(defmacro sync-rpc (message
                    (result-place-or-list destination
                     &key
                       returned?
                       (message-type 'message-rpc-done)
                       (message-unpacker 'message-rpc-done-result))
                    &body body)
  "Performs a synchronized RPC call.  Only allowed inside the body of DEFINE-PROCESS-UPKEEP.

Sends `MESSAGE' to `DESTINATION', waits for a reply (of type `MESSAGE-TYPE'), and unpacks the reply (using `MESSAGE-UNPACKER') into `RESULT-PLACE-OR-LIST'.

If `RETURNED?' is supplied and this call generates a `MESSAGE-RTS' reply, then `RETURNED?' will be flagged and control resumes.  Otherwise, controlled is interrupted by an error."
  (multiple-value-bind (body decls) (a:parse-body body)
    (a:with-gensyms (listen-channel message-place our-message)
      (labels
          ((ignorables ()
             (etypecase result-place-or-list
               (symbol
                `((declare (ignorable ,result-place-or-list))))
               (list
                `((declare (ignorable ,@result-place-or-list))))))
           (body (rts)
             `(,@decls
               ,@(ignorables)
               (unregister ,listen-channel)
               ,@(when returned? `((setf ,returned? ,rts)))
               ,@body)))
        `(let* ((,listen-channel (register))
                (,our-message (copy-structure ,message))
                ,@(unless (null returned?) `(,returned?)))
           ,@(unless (null returned?) `((declare (ignorable ,returned?))))
           (setf (message-reply-channel ,our-message) ,listen-channel)
           (send-message ,destination ,our-message)
           (sync-receive (,listen-channel ,message-place)
             ,@(unless (null returned?)
                 `((message-RTS
                    (lax-destructuring-bind
                        ,result-place-or-list
                        ,(etypecase result-place-or-list
                           (symbol 'nil)
                           (list `(list ,@(mapcar (constantly nil) result-place-or-list))))
                      ,@(body t)))))
             (,message-type
              (lax-destructuring-bind
                  ,result-place-or-list
                  (,message-unpacker ,message-place)
                ,@(body nil)))))))))
