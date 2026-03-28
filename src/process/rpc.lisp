;;;; rpc.lisp
;;;;
;;;; This is a thin wrapper over the bare messaging framework in support of
;;;; common RPC idioms.  It is _not_ comprehensive.

(in-package #:aether)

(defun finish-rpc-handler (&optional return-value (handled t))
  "Early escape from DEFINE-RPC-HANDLER.  Like FINISH-HANDLER, but with a retval."
  (declare (ignore return-value handled))
  (error () "Cannot call FINISH-RPC-HANDLER outside of a DEFINE-RPC-HANDLER body."))

;; TODO: this traps RETURN-FROM, but not FINISH-WITH-SCHEDULING.
(defmacro define-rpc-handler (((process process-type) (message message-type)
                               &key (guard nil guard-p))
                              &body body)
  "Interrupt-based RPC handlers are expected to emit a reply to the caller.  This macro augments DEFINE-MESSAGE-HANDLER to reply to the caller with the last evaluated form."
  (a:with-gensyms (block-name return-value reply-channel handled)
    `(define-message-handler
         ((,process ,process-type) (,message ,message-type)
          ,@(when guard-p `(:guard ,guard)))
       (multiple-value-bind (,return-value ,handled)
           (block ,block-name
             (flet ((finish-handler (&optional ,return-value (,handled t))
                      (return-from ,block-name (values ,return-value ,handled))))
               (declare (ignorable #'finish-handler))
               (values (progn ,@body) t)))
         (a:when-let ((,reply-channel (message-reply-channel ,message)))
           (send-message ,reply-channel (make-message-rpc-done :result ,return-value)))
         (finish-handler ,handled)))))

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
