;;;; event.lisp
;;;;
;;;; DSL for simulating an underdetermined, time-ordered family of events.

(in-package #:aether)

;;;
;;; special variables associated to an active simulation heap
;;;

(defvar *scheduling-accumulator*)
(setf (documentation '*scheduling-accumulator* 'symbol)
      "Temporary housing for events accumulated during WITH-SCHEDULING.")

(defvar *scheduling-clock*)
(setf (documentation '*scheduling-clock* 'symbol)
      "Time at which the WITH-SCHEDULING block is running.")

(declaim (inline now))  ; Poor man's read-only variable.
(defun now ()
  "Returns the current time in the ambient simulation."
  *scheduling-clock*)

;;;
;;; event loop manager, its data types, utilities, *not* the actual loop
;;;

(defstruct event
  "Wrapper for a particular event."
  (callback nil :read-only t)
  (time       0 :read-only t :type (rational 0)))

(defgeneric handle-object (object)
  (:documentation "Describes the generic behavior of OBJECTs of a particular type, as occuring at a particular TIME."))

(defstruct (simulation (:constructor %make-simulation))
  "Container for a set of EVENTs to be processed as part of a simulation.

CONTENTS: The set of EVENTs.  This field is not for direct access; instead, add events using SIMULATION-ADD-EVENT.

HORIZON: The timestamp before which all events have been simulated.  Ensures that a SIMULATION has a fixed history."
  (contents (make-cheap-heap) :type cheap-heap)
  (horizon  0   :type (real 0)))

(defun make-simulation ()
  "Constructs an empty SIMULATION."
  (%make-simulation :contents (make-cheap-heap)))

(defun simulation-add-event (simulation event)
  "Installs EVENT into SIMULATION for later processing."
  (check-type simulation simulation)
  (check-type event event)
  (assert (<= (simulation-horizon simulation) (event-time event))
          nil
          "Simulation ~a has horizon ~a, but event ~a is at past time ~a < ~a"
          simulation (simulation-horizon simulation)
          event (event-time event) (simulation-horizon simulation))
  (cheap-heap-enqueue (simulation-contents simulation)
                      event (event-time event))
  simulation)

(defun simulation-peep (simulation)
  "Reveals the next EVENT that SIMULATION will return upon DEQUEUE."
  (cheap-heap-peep (simulation-contents simulation)))

(defun simulation-dequeue (simulation)
  "Removes the next EVENT from SIMULATION and returns it."
  (initialize-and-return
      ((event (cheap-heap-dequeue (simulation-contents simulation))))
    (setf (simulation-horizon simulation) (event-time event))))

(defmacro with-simulation ((name (&rest starting-objects)) &body body)
  "Initializes a new SIMULATION bound to name NAME using MAKE-SIMULATION and adds an EVENT to the SIMULATION at time 0 for every object in STARTING-OBJECTs."
  `(let ((,name (make-simulation)))
     ,@(loop :for object :in starting-objects
             :collect `(simulation-add-event ,name (make-event :callback ,object
                                                               :time 0)))
     ,@body))

;;;
;;; DSL for building event producers associated to object types
;;;

(defun schedule (fn-or-obj time)
  "Installs a future EVENT object, which invokes FN-OR-OBJ at the specified TIME."
  (push (make-event :callback fn-or-obj
                    :time time)
        *scheduling-accumulator*)
  (values))

(defun schedule* (events)
  "Installs a list of future EVENT objects."
  (setf *scheduling-accumulator*
        (append events *scheduling-accumulator*))
  (values))

(defmacro with-scheduling (now &body body)
  "Stores an implicit set of EVENT objects to return when exiting the WITH-SCHEDULING block.

Provides some helper functions: SCHEDULE, SCHEDULE*, and FINISH-WITH-SCHEDULING."
  (alexandria:with-gensyms (block-name last-value)
    `(let ((*scheduling-accumulator* nil)
           (*scheduling-clock* ,now)
           ,last-value)
       (block ,block-name
         (labels ((finish-with-scheduling (&optional (events nil eventsp))
                    (return-from ,block-name
                      (if eventsp events *scheduling-accumulator*))))
           (declare (ignorable #'finish-with-scheduling))
           (setf ,last-value (progn ,@body))
           (values *scheduling-accumulator* ,last-value))))))

;;;
;;; some standard canaries
;;;

(defun canary-until (until)
  "Pause a simulation after UNTIL passes."
  (lambda () (< until (now))))

(defun canary-timeout (timeout)
  "Throw an error when TIMEOUT arrives."
  (lambda ()
    (when (<= timeout (now))
      (error ""))))

(defun canary-process (process)
  "Pause a simulation when PROCESS halts."
  (lambda () (endp (process-command-stack process))))

(defun canary-any (&rest canaries)
  "Announce a trigger when any one of the CANARIES is triggered."
  (lambda ()
    (loop :for canary :in canaries
            :thereis (funcall canary))))

(defun canary-all (&rest canaries)
  "Announce a trigger when all of the CANARIES are simultaneously triggered."
  (lambda ()
    (loop :for canary :in canaries
          :always (funcall canary))))

;;;
;;; event loop and basic handlers
;;;

(defmacro with-active-simulation (simulation &body body)
  "Binds the simulation special variables to SIMULATION.  Useful for sending messages into a simulation from 'outside' of it."
  (a:once-only ((simulation simulation))
    (a:with-gensyms (event events last-value)
      `(multiple-value-bind (,events ,last-value)
           (with-scheduling (simulation-horizon ,simulation)
             ,@body)
         (dolist (,event ,events ,last-value)
           (check-type ,event event)
           (simulation-add-event ,simulation ,event))))))

(defun simulation-run (simulation &key canary)
  "Processes EVENTs belonging to SIMULATION, until either SIMULATION is exhausted or CANARY evaluates to T."
  (check-type simulation simulation)
  (check-type canary (or null function))
  (let ((event (simulation-peep simulation)))
    ;; no more events? return NIL to say we're done
    (unless event
      (return-from simulation-run nil))
    ;; more events but the canary is dead? return SIMULATION to say we're done
    (let ((*scheduling-clock* (event-time event)))
      (when (and canary (funcall canary))
        (return-from simulation-run simulation)))
    ;; otherwise: deal with the event
    (simulation-dequeue simulation)
    (with-scheduling (event-time event)
      (typecase (event-callback event)
        (function  (funcall (event-callback event)))
        (otherwise (handle-object (event-callback event))))
      (dolist (new-event *scheduling-accumulator*)
        (check-type new-event event)
        (simulation-add-event simulation new-event)))
    (simulation-run simulation :canary canary)))

(defmacro define-object-handler (((object-variable object-type)) &body body)
  "Defines a default event handler for OBJECT-TYPE."
  (multiple-value-bind (forms decls docstring) (alexandria:parse-body body)
    `(defmethod handle-object ((,object-variable ,object-type))
       ,@(when docstring (list docstring))
       ,@(when decls decls)
       (block nil ,@forms))))

;; Here lies an implementation of SIMULATION on top of a bare CL-HEAP.  It's
;; very slow and memory-expensive in our use case!
#+#:pedagogy-only
((deftype simulation ()
   'cl-heap:priority-queue)

 (defun make-simulation ()
   "Constructs an empty SIMULATION."
   (make-instance 'cl-heap:priority-queue))

 (defun simulation-add-event (simulation event)
   "Installs EVENT into SIMULATION for later processing."
   (check-type simulation simulation)
   (check-type event event)
   (cl-heap:enqueue simulation event (event-time event))
   simulation)

 (defun simulation-peep (simulation)
   "Reveals the next EVENT that SIMULATION will return upon DEQUEUE."
   (cl-heap:peep-at-queue simulation))

 (defun simulation-dequeue (simulation)
   "Removes the next EVENT from SIMULATION and returns it."
   (cl-heap:dequeue simulation)))
