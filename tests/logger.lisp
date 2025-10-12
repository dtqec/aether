;;;; tests/logger.lisp
;;;;
;;;; Basic tests of logging functionality.

(in-package #:aether-tests)

(deftest test-log-trimming ()
  (with-courier ()
    (let* ((process (spawn-process 'process))
           (channel (address-channel (process-public-address process)))
           (address (aether::make-address :channel channel))
           (*logger* (aether::make-logger :entries `((:source ,process :time 1))))
           (entries (logger-entries *logger*)))
      (is (equal (trim-log :start-time 0.5) entries))
      (is (equal (trim-log :end-time 2) entries))
      (is (equal (trim-log :start-time 0.7 :end-time 1.2) entries))
      (is (endp (trim-log :start-time 1.2)))
      (is (endp (trim-log :start-time 1.2 :end-time 2)))
      (is (equal (logs-for-process process) entries))
      (is (equal (logs-for-address address) entries))
      (is (equal (logs-for-channel channel) entries))
      (is (equal (trim-log :start-time 0.5
                           :end-time 2
                           :entries (logs-for-process
                                     process
                                     (logs-for-address
                                      address
                                      (logs-for-channel channel))))
                 entries)))))

(deftest test-log-conditions ()
  (with-transient-logger (:log-level 1 :start-time 10 :end-time 20)
    ;; conditions baked into with-transient-logger
    (log-entry :source nil :log-level 0 :time 0  :entry-type ':a) ; too early
    (log-entry :source nil :log-level 0 :time 15 :entry-type ':b) ; good
    (log-entry :source nil :log-level 0 :time 20 :entry-type ':c) ; good
    (log-entry :source nil :log-level 1 :time 15 :entry-type ':d) ; good
    (log-entry :source nil :log-level 2 :time 15 :entry-type ':e) ; too high level
    ;; mute all log entries
    (only-log-when ((entry) (declare (ignore entry)) nil)
      (log-entry :source nil :log-level 0 :time 15 :entry-type ':f)) ; muted
    ;; only-log-when delays condition evaluation til log-entry is called
    (let ((*log-level* 0))
      (log-entry :source nil :log-level 0 :time 15 :entry-type ':g)  ; good
      (log-entry :source nil :log-level 1 :time 15 :entry-type ':h)) ; newly too high level
    ;; only-log-when intersects conditions, doesn't replace them
    (only-log-when ((entry) (<= 1 (getf entry ':log-level)))
      (log-entry :source nil :log-level 0 :time 15 :entry-type ':i)  ; too low level!
      (log-entry :source nil :log-level 1 :time 15 :entry-type ':j)  ; good
      (log-entry :source nil :log-level 2 :time 15 :entry-type ':k)) ; too high level
    (is (equalp '(:j :g :d :c :b)
                (mapcar (lambda (e) (getf e ':entry-type)) (logger-entries *logger*)))))
  (values))
