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
