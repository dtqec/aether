;;;; package.lisp

#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(defpackage #:aether
  (:use #:cl)
  
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))

  ;; symbols for export
  
  ;; utilities.lisp
  (:export
   #:initialize-and-return              ; MACRO
   #:dohash                             ; MACRO
   #:destructuring-places               ; MACRO
   #:ignorant-lambda                    ; MACRO
   #:peek                               ; FUNCTION
   )
  
  ;; queue.lisp
  (:export
   #:make-q                             ; FUNCTION
   #:q-empty                            ; FUNCTION
   #:q-enq                              ; FUNCTION
   #:q-deq                              ; FUNCTION
   #:q-peek                             ; FUNCTION
   #:q-len                              ; FUNCTION
   #:q-deq-first                        ; FUNCTION
   #:q-deq-when                         ; FUNCTION
   #:q-push                             ; FUNCTION
   )
  
  ;; cheap-heap.lisp
  (:export
   #:make-cheap-heap                    ; FUNCTION
   #:cheap-heap-enqueue                 ; FUNCTION
   #:cheap-heap-dequeue                 ; FUNCTION
   #:cheap-heap-peep                    ; FUNCTION
   #:cheap-heap-empty?                  ; FUNCTION (PREDICATE)
   #:cheap-heap->list                   ; FUNCTION
   )
  
  ;; logger.lisp
  (:export
   #:log-entry                          ; FUNCTION
   #:with-transient-logger              ; MACRO
   #:*logger*                           ; PARAMETER
   #:reset-logger                       ; FUNCTION
   #:logger-entries                     ; FUNCTION
   #:print-log                          ; FUNCTION
   #:print-message-report               ; FUNCTION
   )
  
  ;; event.lisp
  (:export
   #:event                              ; TYPE
   #:simulation                         ; TYPE
   #:with-simulation                    ; MACRO
   
   #:make-event                         ; FUNCTION
   #:make-simulation                    ; FUNCTION
   #:simulation-add-event               ; FUNCTION
   #:simulation-run                     ; FUNCTION
   #:simulation-horizon                 ; FUNCTION (ACCESSOR)
   
   #:with-scheduling                    ; MACRO
   #:schedule                           ; FUNCTION
   #:schedule*                          ; FUNCTION
   #:finish-with-scheduling             ; MACRO
   #:with-active-simulation             ; MACRO
   
   #:canary-until                       ; FUNCTION
   #:canary-timeout                     ; FUNCTION
   #:canary-process                     ; FUNCTION
   #:canary-any                         ; FUNCTION
   #:canary-all                         ; FUNCTION
   
   ;; DEPRECATED SYMBOLS
   #:with-futures                       ; MACRO
   #:future                             ; FUNCTION
   #:future*                            ; FUNCTION
   #:finish-with-futures                ; MACRO
   )
  
  ;; message.lisp
  (:export
   #:*local-courier*                    ; PARAMETER
   #:courier                            ; TYPE
   #:private-key                        ; TYPE
   #:address                            ; TYPE
   #:address=                           ; FUNCTION
   #:hash-address
   #:make-courier                       ; FUNCTION
   #:courier-processing-clock-rate      ; FUNCTION
   #:with-courier                       ; MACRO
   
   #:message                            ; STRUCTURE
   #:message-RTS                        ; STRUCTURE
   #:make-message                       ; FUNCTION
   #:make-message-RTS                   ; FUNCTION
   #:reply-channel                      ; SYMBOL
   #:message-reply-channel              ; ACCESSOR
   #:register                           ; FUNCTION
   #:send-message                       ; FUNCTION
   #:send-message-batch                 ; FUNCTION
   #:receive-message                    ; MACRO
   #:unregister                         ; FUNCTION
   )
  
  ;; network.lisp
  (:export
   #:make-courier-grid                  ; FUNCTION
   )
  
  ;; process/
  (:export
   #:process                            ; CLASS
   #:process-command-stack              ; ACCESSOR
   #:process-data-stack                 ; ACCESSOR
   #:process-public-address             ; ACCESSOR
   #:process-die                        ; FUNCTION
   #:process-clock-rate                 ; SYMBOL
   #:process-debug?                     ; ACCESSOR
   #:process-exhaust-inbox?             ; ACCESSOR
   #:process-peruse-inbox?              ; ACCESSOR
   #:START                              ; SYMBOL

   #:define-message-dispatch            ; MACRO
   #:define-process-upkeep              ; MACRO
   #:define-object-handler              ; MACRO
   #:define-message-handler             ; MACRO
   #:sync-receive                       ; MACRO
   #:process-continuation               ; FUNCTION
   
   #:with-address-dereferencing         ; MACRO
   #:dereference                        ; FUNCTION
   #:spawn-process                      ; FUNCTION

   #:define-message-subordinate         ; MACRO
   
   #:wake-on-network                    ; FUNCTION
   #:finish-handler                     ; MACRO
   )
  
  ;; rpc.lisp
  (:export
   #:message-rpc-done                   ; STRUCTURE
   #:make-message-rpc-done              ; FUNCTION
   #:message-rpc-done-result            ; ACCESSOR
   #:define-rpc-handler                 ; MACRO
   #:sync-rpc                           ; MACRO
   #:with-replies                       ; MACRO
   )
  
  ;; lock.lisp
  (:export
   #:process-lockable                   ; CLASS
   #:process-lockable-targets           ; GENERIC FUNCTION
   #:process-lockable-locked?           ; ACCESSOR
   #:process-lockable-aborting?         ; ACCESSOR
   #:process-lockable-done-signal       ; ACCESSOR
   #:aborting?                          ; SYMBOL
   #:done-signal                        ; SYMBOL
   #:downward-rx-latches                ; SYMBOL
   #:upward-rx-latch                    ; SYMBOL
   #:upward-tx-latch                    ; SYMBOL
   #:downward-tx-latches                ; SYMBOL
   
   #:message-lock                       ; STRUCTURE
   #:make-message-lock                  ; FUNCTION
   #:make-message-unlock                ; FUNCTION
   
   #:handle-message-lock                ; FUNCTION
   #:BROADCAST-LOCK                     ; SYMBOL
   #:START-LOCK                         ; SYMBOL
   #:BROADCAST-UNLOCK                   ; SYMBOL
   )

  ;; cast.lisp
  (:export
   #:return-from-cast                   ; FUNCTION

   #:broadcast-frame                    ; STRUCTURE
   #:make-broadcast-frame               ; FUNCTION
   #:push-broadcast-frame               ; FUNCTION
   #:define-broadcast-handler           ; MACRO

   #:convergecast-frame                 ; STRUCTURE
   #:make-convergecast-frame            ; FUNCTION
   #:push-convergecast-frame            ; FUNCTION
   #:define-convergecast-handler        ; MACRO
   ))
