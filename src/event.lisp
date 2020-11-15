;;;; event.lisp
;;;;
;;;; DSL for simulating an underdetermined, time-ordered family of events.

(in-package #:aether)

;;;
;;; event loop manager and its data types
;;;

(defstruct event
  "Wrapper for a particular event."
  (callback nil :read-only t)
  (time       0 :read-only t :type (rational 0)))

(defgeneric handle-object (object time)
  (:documentation "Describes the generic behavior of OBJECTs of a particular type, as occuring at a particular TIME."))

(defstruct (simulation (:constructor %make-simulation))
  "Container for a set of EVENTs to be processed as part of a simulation.

CONTENTS: The set of EVENTs.  This field is not for direct access; instead, add events using SIMULATION-ADD-EVENT.

HORIZON: The timestamp before which all events have been simulated.  Ensures that a SIMULATION has a fixed history."
  (contents nil :type list)
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

(defun simulation-run (simulation &key canary)
  "Processes EVENTs belonging to SIMULATION, until either SIMULATION is exhausted or CANARY evaluates to T."
  (check-type simulation simulation)
  (check-type canary (or null function))
  (let ((event (simulation-peep simulation)))
    ;; no more events? return NIL to say we're done
    (unless event
      (return-from simulation-run nil))
    ;; more events but the canary is dead? return SIMULATION to say we're done
    (when (and canary (funcall canary (event-time event)))
      (return-from simulation-run simulation))
    ;; otherwise: deal with the event
    (simulation-dequeue simulation)
    (let ((simulation-results
            (typecase (event-callback event)
              (function (funcall (event-callback event) (event-time event)))
              (otherwise (handle-object (event-callback event) (event-time event))))))
      (check-type simulation-results list)
      (dolist (new-event simulation-results)
        (check-type new-event event)
        (simulation-add-event simulation new-event))
      (simulation-run simulation :canary canary))))

(defmacro with-simulation ((name (&rest starting-objects)) &body body)
  "Initializes a new SIMULATION bound to name NAME using MAKE-SIMULATION and adds an EVENT to the SIMULATION at time 0 for every object in STARTING-OBJECTs."
  `(let ((,name (make-simulation)))
     ,@(loop :for object :in starting-objects
             :collect `(simulation-add-event ,name (make-event :callback ,object
                                                               :time 0)))
     ,@body))

;;; some standard canaries

(defun canary-until (until)
  "Pause a simulation after UNTIL passes."
  (lambda (now) (< until now)))

(defun canary-timeout (timeout)
  "Throw an error when TIMEOUT arrives."
  (lambda (now)
    (when (<= timeout now)
      (error ""))))

(defun canary-process (process)
  "Pause a simulation when PROCESS halts."
  (lambda (now) (endp (process-command-stack process))))

(defun canary-any (&rest canaries)
  "Announce a trigger when any one of the CANARIES is triggered."
  (lambda (now)
    (loop :for canary :in canaries
          :thereis (funcall canary now))))

(defun canary-all (&rest canaries)
  "Announce a trigger when all of the CANARIES are simultaneously triggered."
  (lambda (now)
    (loop :for canary :in canaries
          :always (funcall canary now))))

;;;
;;; DSL for building event producers associated to object types
;;;

(defmacro with-futures (&body body)
  "Stores an implicit set of EVENT objects to return when exiting the WITH-FUTURE block.

Provides some helper functions: FUTURE, FUTURE*, and FINISH-WITH-FUTURES."
  (alexandria:with-gensyms (block-name futures-collection last-value)
    `(let (,futures-collection ,last-value)
       (block ,block-name
         (labels ((future (fn-or-obj time)
                    (push (make-event :callback fn-or-obj
                                      :time time)
                          ,futures-collection)
                    (values))
                  (future* (events)
                    (setf ,futures-collection
                          (append events ,futures-collection))
                    (values))
                  (finish-with-futures (&optional (events nil eventsp))
                    (return-from ,block-name
                      (if eventsp events ,futures-collection))))
           (declare (ignorable #'future #'future* #'finish-with-futures))
           (setf ,last-value (progn ,@body))
           (values ,futures-collection ,last-value))))))

(defmacro define-object-handler (((object-variable object-type) time-variable) &body body)
  "Defines a default event handler for OBJECT-TYPE."
  (multiple-value-bind (forms decls docstring) (alexandria:parse-body body)
    `(defmethod handle-object ((,object-variable ,object-type) ,time-variable)
       ,@(when docstring (list docstring))
       ,@(when decls decls)
       (with-futures
         ,@forms))))

(defun future (fn-or-obj time)
  "Installs a future EVENT object, which invokes FN-OR-OBJ at the specified TIME."
  (declare (ignore fn-or-obj time))
  (error "FUTURE is not defined outside of WITH-FUTURES."))

(defun future* (event-list)
  "Installs a list of future EVENT objects."
  (declare (ignore event-list))
  (error "FUTURE* is not defined outside of WITH-FUTURES."))

(defun finish-with-futures (&optional event-list)
  "Breaks out of WITH-FUTURE. If EVENTS is supplied, returns EVENTS in place of the implicit event list."
  (declare (ignore event-list))
  (error "FINISH-WITH-FUTURES is not defined outside of WITH-FUTURES."))



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
