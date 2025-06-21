;;;; courier.lisp
;;;;
;;;; Implements a message transmission system.

(in-package #:aether)

;;;
;;; What the user thinks of as primitive operations on messages are actually
;;; interactions with the local courier, stashed in the dynamic variable
;;; `*LOCAL-COURIER*'.
;;;

(defparameter *local-courier* nil
  "Bound to the `COURIER' that services this process.")
(defparameter *courier-processing-clock-rate* 100)
(defparameter *routing-time-step* 1/100)

;;;
;;; global counters to give unique IDs
;;;

(define-global-counter **courier-index** get-courier-index)
(define-global-counter **channel-index** get-channel-index)
(defun get-secret-index ()
  (random most-positive-fixnum))

;;;
;;; couriers
;;;

(defstruct courier
  "A component in the message-passing apparatus.

`QUEUE': The receive queue of messages which have not yet been forwarded or stored.  Each entry is of the form (COURIER-ID CHANNEL &REST PAYLOAD).
`INBOXES': A hash table mapping channels serviced by this courier to mailbox queues.
`SECRETS': A hash table mapping channels serviced by this courier to the private sigils used to distinguish mailbox owners.
`ID': A unique identifier for this courier. WARNING: Expect subclasses of `COURIER' to require specific types and values here.
`NEIGHBORS': Used to store routing information. WARNING: By default, this is a hash mapping courier `ID's in the network to their object instances. Expect subclasses of `COURIER' to install different types and values here.
`ASLEEP-SINCE': Couriers which are not processing inbound messages fall asleep and are temporarily removed from the event heap.  This slot records the last time at which this courier had its event handler processed.
`LISTENERS': Mapping from inboxes to lists of processes to `WAKE-UP' when a new message arrives."
  (queue (make-q)) ; messages not yet sorted
  (inboxes (make-hash-table :test 'eq))
  (secrets (make-hash-table :test 'eq))
  (processing-clock-rate *courier-processing-clock-rate*)
  (default-routing-time-step *routing-time-step*)
  (id (get-courier-index))
  (neighbors (make-hash-table))
  (asleep-since nil)  ; :type (or nil 'fraction)
  (listeners (make-hash-table :test 'eq)))

(defmethod print-object ((object courier) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defmacro with-courier ((&rest keyword-arguments) &body body)
  "Initializes the `*LOCAL-COURIER*' parameter as a fresh `COURIER'. Also takes a list of `KEYWORD-ARGUMENT's (e.g. :processing-clock-rate 20) that are passed along to the constructor."
  `(let ((*local-courier* (make-courier ,@keyword-arguments)))
     ,@body))

;;;
;;; primitive operations on addresses / couriers
;;;

;; TODO: i would like to make it "illegal" to send private keys across messages.
;;       only "public addresses" should be legal. it would be best, i think, if
;;       private keys were automatically downgraded to public addresses.
(defun register (&key (courier *local-courier*)
                      (channel (get-channel-index))
                      (secret (get-secret-index)))
  "Registers a fresh channel over which messages can be transmitted and received."
  (assert (null (gethash channel (courier-inboxes courier))) ()
          "Channel name already in use: ~a" channel)
  (setf (gethash channel (courier-inboxes courier)) (make-q)
        (gethash channel (courier-secrets courier)) secret)
  (make-address :courier (courier-id courier)
                :channel channel
                :secret secret))

(defun unregister (address)
  "Unregisters a channel, so that messages can no longer be transmitted or received over it."
  (with-slots (courier channel secret) address
    (declare (ignore courier))
    ;; check that the caller has privileged access
    (when (eq secret (gethash channel (courier-secrets *local-courier*)))
      ;; return lingering messages to sender
      (loop :until (q-empty (gethash channel (courier-inboxes *local-courier*)))
            :for message := (q-deq (gethash channel (courier-inboxes *local-courier*)))
            :for reply-channel := (message-reply-channel message)
            :when reply-channel
              :do (send-message reply-channel (make-message-RTS)))
      ;; expunge the mailbox
      (remhash channel (courier-secrets *local-courier*))
      (remhash channel (courier-inboxes *local-courier*))
      (remhash channel (courier-listeners *local-courier*))
      (values))))

;;;
;;; utility (non-exported) operations on messages / couriers
;;;

(defun stash-local-message (message)
  "Attempts to store `MESSAGE' into a mailbox at `*LOCAL-COURIER*'.  Returns NIL if `MESSAGE' is not bound for `*LOCAL-COURIER*', T otherwise."
  (destructuring-bind (destination channel &rest payload) message
    (unless (equal destination (courier-id *local-courier*))
      (return-from stash-local-message nil))
    (let ((inbox (gethash channel (courier-inboxes *local-courier*))))
      (cond
        ((null inbox)
         (a:when-let ((reply-channel (message-reply-channel payload)))
           (send-message reply-channel (make-message-RTS))))
        (t
         (dolist (sleeper (gethash channel (courier-listeners *local-courier*)))
           (wake-up sleeper))
         (setf (gethash channel (courier-listeners *local-courier*)) nil)
         (q-enq payload inbox)))
      t)))

(defun check-key-secret (address)
  "Asserts that the SECRET stored in ADDRESS matches that recorded by the attendant COURIER."
  (assert (equal (address-courier address)
                 (courier-id *local-courier*))
          ()
          "ADDRESS must belong to *LOCAL-COURIER*.")
  (assert (nth-value 1 (gethash (address-channel address) (courier-secrets *local-courier*)))
          ()
          "Local ADDRESS is not registered to *LOCAL-COURIER*.")
  (assert (eql (address-secret address)
               (gethash (address-channel address)
                        (courier-secrets *local-courier*)))
          ()
          "ADDRESS's secret did not match courier's secret."))

(defun deliver-message (processing-courier message)
  "Used to simulate the transmission of a message to the next COURIER."
  (wake-up processing-courier)
  (q-enq message (courier-queue processing-courier))
  (values))

(defgeneric courier-courier->route (processing-courier destination-address)
  (:documentation "Calculates the next step from `PROCESSING-COURIER' to `DESTINATION-ADDRESS'.  Returns as an optional second value the time delay between transmission at `PROCESSING-COURIER' to receipt by the next hop.")
  (:method ((processing-courier courier) destination-address)
    (multiple-value-bind (destination found?)
        (gethash destination-address (courier-neighbors processing-courier))
      (unless found?
        (error "I don't know how to route."))
      destination)))

;;;
;;; primitive operations on messages / couriers
;;;

(defun send-message (destination payload)
  "Sends the message `PAYLOAD' to be received at `DESTINATION', an `ADDRESS'.  Returns the `REPLY-CHANNEL' of the `PAYLOAD', if any."
  (check-type destination address)
  (check-type payload message)
  (assert (not (null *local-courier*))
          ()
          "SEND-MESSAGE not permitted without a local courier.")
  (let ((packed-payload (list* (address-courier destination)
                               (address-channel destination)
                               payload)))
    (unless (stash-local-message packed-payload)
      (deliver-message *local-courier* packed-payload))
    (message-reply-channel payload)))

(defun send-message-batch (payload-constructor destinations &key (replies? t))
  "Sends a batch of messages to the `ADDRESS'es housed in `DESTINATIONS'.  Each message is constructed afresh using `PAYLOAD-CONSTRUCTOR', supplied with a freshly registered reply channel, and the list of reply channels is returned as a result."
  (check-type destinations list)
  (loop :for destination :in destinations
        :for reply-address := (when replies? (register))
        :for message := (funcall payload-constructor)
        :do (setf (message-reply-channel message) reply-address)
            (send-message destination message)
        :collect reply-address))

;; TODO: behaves like TIMEOUT = 0. someday permit TIMEOUT > 0?
(defmacro receive-message ((address
                            message
                            &key
                              (timeout 0)
                              (catch-RTS? t)
                              (peruse-inbox? t)
                            &allow-other-keys)
                           &body clauses)
  "Peruses the mailbox at `ADDRESS' for a `MESSAGE' which matches one of the provided `CLAUSES'.  Each clause has the form (MESSAGE-TYPE &BODY BODY).  Clauses are processed according to the following Erlang-ian rules:

  + Each clause is processed in the order supplied.
  + If a clause is matched, no further clauses are processed.
  + When `PERUSE-INBOX?' is T, each clause (processed in order) searches the whole inbox(in latest-to-most-recent order) for a `MESSAGE-TYPE' match. When NIL, each clause just looks at the first message in the inbox for a `MESSAGE-TYPE' match.
  + If a waiting message of the appropriate type is found, it is bound to `MESSAGE' and `BODY' is processed.

NOTES:

  When `CATCH-RTS?' is T, we append a `MESSAGE-RTS' clause that throws an error.

  Permits a clause with head `OTHERWISE' which is executed when no such waiting message is found.

  Returns as a secondary value whether a message was processed.  (An `OTHERWISE' clause also results in a secondary value of NIL.)"
  (when catch-RTS?
    (setf clauses (append clauses `((message-RTS (error "Got an RTS."))))))
  (assert (zerop timeout) () "Blocking RECEIVE-MESSAGE not currently supported.")
  (a:with-gensyms (block-name inbox found? q-deq-fn)
    (flet ((process-clause (clause-head clause-body)
             `(a:when-let ((,message
                            (funcall ,q-deq-fn ,inbox
                                     (lambda (m) (typep m ',clause-head)))))
                (return-from ,block-name
                  (values (progn ,@clause-body)
                          t)))))
      `(block ,block-name
         (policy-cond:policy-cond
          ((= 3 safety)
            (check-key-secret ,address))
          ((> 3 safety)
            nil))
         (let ((,q-deq-fn (if ,peruse-inbox? #'q-deq-first #'q-deq-when)))
           (multiple-value-bind (,inbox ,found?)
               (gethash (address-channel ,address)
                        (courier-inboxes *local-courier*))
             (declare (ignorable ,inbox))
             (assert ,found? ()
                     "Address ~a not registered to this courier."
                     (address-channel ,address))
             ,@(loop :for (clause-head . clause-body) :in clauses
                     :unless (eql 'otherwise clause-head)
                       :collect (process-clause clause-head clause-body))
             (values (progn ,@(cdr (find 'otherwise clauses :key #'car)))
                     nil)))))))

;;;
;;; event producers for message passing infrastructure
;;;

(define-object-handler ((courier courier))
  "Processes messages in the COURIER's I/O queue: messages bound for other COURIERs get forwarded, and messages bound for this COURIER get sorted into local mailboxes."
  (when (q-empty (courier-queue courier))
    (setf (courier-asleep-since courier) (now))
    (return))
  (let ((message (q-deq (courier-queue courier)))
        (*local-courier* courier))
    (cond
      ;; are we this message's destination?
      ((stash-local-message message)
       (schedule courier (now)))
      ;; otherwise, route it
      (t
       (multiple-value-bind (intermediate-destination time-to-deliver)
           (courier-courier->route courier (first message))
         (setf time-to-deliver (or time-to-deliver
                                   (courier-default-routing-time-step courier)))
         (schedule (ignorant-lambda
                     (deliver-message intermediate-destination message))
                   (+ (now) time-to-deliver))
         (schedule courier (+ (now) (/ (courier-processing-clock-rate courier)))))))))

(defgeneric wake-up (courier)
  (:documentation "If this actor has previously fallen asleep and was removed from the simulation heap, this re-inserts it.  (No action if the actor is already awake.)")
  (:method ((courier courier))
    (a:when-let* ((since (courier-asleep-since courier))
                  (next-tick (+ since
                                (/ (ceiling (- (now) since)
                                            (/ (courier-processing-clock-rate courier)))
                                   (courier-processing-clock-rate courier)))))
      (setf (courier-asleep-since courier) nil)
      (schedule courier next-tick))))
