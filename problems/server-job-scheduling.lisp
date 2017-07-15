(in-package :cl-rl-problems)

(defclass server-job-scheduling (markov-decision-process)
  ((queue-size :initarg :queue-size
               :type integer
               :reader queue-size
               :documentation "Number of jobs in the queue"))
  (:documentation "State is represented by queue of jobs"))

(defclass job ()
  ((type :initarg :type
         :initform 0
         :type integer
         :reader job-type
         :documentation "Jobs differ by type. Each type has its own utility function")
   (utility :initarg :utility
            :initform (lambda (time) (- 100 time))
            :reader job-utility
            :documentation "Function of time from job arrival to its exectution. The sum of these should be maximized by an agent.")
   (age :initarg :age
        :initform 0
        :accessor job-age)))

(defparameter *next-job-type* 0)
(defparameter *job-types* (make-hash-table :test #'equal))

(defun generate-job (max-time min-val)
  (labels ((%1 (max-time min-val) "\_"
               (lambda (time)
                 (if (< time max-time)
                     (/ time min-val)
                     min-val))))
    (let ((type (aif (gethash (list max-time min-val) *job-types*)
                     it
                     (setf (gethash (list max-time min-val) *job-types*)
                           (incf *next-job-type*)))))
      (make-instance 'job
                     :type type
                     :utility (%1 max-time min-val)
                     :age 0))))

(defmethod possible-actions ((mdp server-job-scheduling) state)
  "This is direct naive approach. Number of possible actions defined by queue size.
   An action is picking particular job for execution. Since `state' is just a list of jobs, return it."
  state)

(defmethod initial-state ((mdp server-job-scheduling))
  (let ((n-types 5)
        (types (mapcar (lambda (pair)
                         (generate-job (car pair) (cdr pair)))
                       (loop :repeat n-types :collect (cons (random 100) (- (random 100)))))))
    (loop :for i :from 0 :to (queue-size mdp) :collect
       (copy-instance ))))
