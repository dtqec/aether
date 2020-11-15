# `aether`

`aether` is a Common Lisp framework for emulating an actor-based distributed system housed on a family of emulated devices.

## Overview

The purpose of `aether` is to provide a testbed in which one can design the behaviors of distributed electronics, emulate software running on them, and instrument the emulation to infer software performance characteristics.

The framework is segmented into the following layers:

* Time-domain simulation via time-triggered events.
* Physical and logical network layers.
* An actor-based DSL.
* A library of standard patterns in distributed programming.

Each layer comes with standard instances which embody default behaviors.
These can be used as-is for "macroscopic", detail-free emulation, and they can be subclassed to permit the injection of detailed, domain-specific behaviors (e.g., faithful emulation of network load).
The actor-based DSL can be useful in the specification of both the behavior of the electronics and the behavior of the program: starting with the latter gives information about algorithm performance in the case of "ideal" hardware, and migrating to the latter pins down the performance features specific to that case.

## Installation

The simplest installation method is to use Quicklisp: running `(ql:quickload "aether")` will download `aether` as well as any project dependencies.

Before `aether` is available via the Quicklisp index, or to use a bleeding-edge version, clone the `aether` git repository into your Quicklisp local project directory and then quickload as above.

## Example

Here is an example description of a simple processor which responds to external impulses (i.e., messages on the `aether` network) to compute integer factorials.

```lisp
(defclass arithmetic-server (process)
  ((process-clock-rate :initform 1))
  (:documentation "Simple server process that handles an arithmetic RPC call."))

;;; processor description / computational primitives

;; all PROCESS instances begin with the command START
(define-process-upkeep ((process arithmetic-server) now) (START)
  "Make the processor sit in an infinite \"listening\" loop."
  (process-continuation process `(START)))

(define-process-upkeep ((process arithmetic-server) now) (PUSH n)
  (push n (process-data-stack process)))

(define-process-upkeep ((process arithmetic-server) now) (MULTIPLY)
  (let ((left (pop (process-data-stack process)))
        (right (pop (process-data-stack process))))
    (push (* left right) (process-data-stack process))))

(define-process-upkeep ((process arithmetic-server) now) (EMIT address)
  (let* ((result (pop (process-data-stack process)))
         (message (make-message-rpc-done :result result)))
    (send-message address message)))

;;; public-facing factorial service

(defstruct (message-factorial (:include message))
  (n   nil :type (integer 0)))

(define-message-handler handle-message-factorial
    ((process arithmetic-server) (message message-factorial) now)
  (process-continuation process
                        `(FACTORIAL ,(message-factorial-n message))
                        `(EMIT ,(message-reply-channel message))))

(define-process-upkeep ((process arithmetic-server) now)
    (FACTORIAL n)
  (cond
    ((zerop n)
     (process-continuation process `(PUSH 1)))
    (t
     (process-continuation process
                           `(FACTORIAL ,(1- n))
                           `(PUSH ,n)
                           `(MULTIPLY)))))

;;; service registry

(define-message-dispatch arithmetic-server
  (message-factorial 'handle-message-factorial))

;;; example: spawning a server and querying it

;; set up the actors
(let* ((simulation (make-simulation))
       (*local-courier* (make-instance 'courier))
       (server (spawn-process 'arithmetic-server))
       (reply-channel (register)))
  (simulation-add-event simulation (make-event :callback server))
  (simulation-add-event simulation (make-event :callback *local-courier*))
  ;; send the factorial request
  (send-message (process-public-address server)
                (make-message-factorial :reply-channel reply-channel
                                        :n 15))
  ;; wait a while for it to complete
  (simulation-run simulation :canary (canary-until 60))
  ;; unpack the reply
  (receive-message (reply-channel done-message)
    (message-rpc-done
     (unregister reply-channel)
     (message-rpc-done-result done-message))))
```

Further examples can be found under the directory `tests/examples/` in the source tree.
They primarily demonstrate features of the standard library.
These "living" examples are run regularly as part of the test suite.

## License

`aether` is made available under the MIT license.
See `LICENSE.md` in the source tree for more information.

## See also

* [ArXiv preprint](https://arxiv.org/abs/2011.06180)
* [GitHub repository](https://github.com/dtqec/aether)
