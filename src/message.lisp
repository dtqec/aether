;;;; message.lisp
;;;;
;;;; The base address and message types.

(in-package #:aether)

;;;
;;; addresses
;;;

(defstruct address
  "Signifies a channel over which objects can communicate.

NOTE: The SECRET field is \"optional\", in that only processes that possess the secret can receive on and unregister the channel, but all processes---including those without the secret---can send on the channel."
  (courier (courier-id *local-courier*))
  (channel)
  (secret))

(defmethod print-object ((object address) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~a" #+ignore (address-courier object) (address-channel object))))

(defun address= (left right)
  "Tests for semantic equality of addresses."
  (and (eql (address-channel left) (address-channel right))
       (equal (address-courier left) (address-courier right))))

(defun public-address (address)
  "Extracts the public (i.e., secret-free) address from a potentially private (i.e., secret-ful) address."
  (initialize-and-return ((public-key (copy-address address)))
    (setf (address-secret public-key) nil)))

(defun hash-address (address)
  (sxhash (address-channel address)))

;;;
;;; standard message types
;;;

(define-global-counter **message-index** get-message-index)

(defstruct message
  "Base type for messages transmitted along `COURIER's."
  (reply-channel nil                 :type (or null address))
  (message-id    (get-message-index) :type integer))

(defstruct (message-RTS (:include message))
  "This message is generated as a fresh automatic response when an original message is sent to a nonexistent (or closing) mailbox.

If the original message's REPLY-CHANNEL is not set, no MESSAGE-RTS object is built.

NOTE: \"RTS\" is short for \"Return To Sender\".")

(defstruct (message-rpc-done (:include message))
  "Reply when an RPC message finishes."
  (result))
