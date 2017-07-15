(in-package :cl-rl-problems)

(defclass random-walk (markov-decision-process)
  ((n :initform 5
      :initarg :n
      :type integer
      :reader walk-n
      :documentation "Number of states"))
  (:documentation "1D random walk"))

(defmethod possible-actions ((walk random-walk) state)
  (declare (ignore state))
  (list t))

(defmethod perform-action ((walk random-walk) state action)
  (declare (ignore action))
  (let* ((next-state (if (= 1 (random 2))
                         (1- state)
                         (1+ state)))
         (reward (if (= (1+ (walk-n walk)) next-state)
                     1.0
                     0.0))) 
    (values next-state reward)))

(defmethod next-states ((walk random-walk) state action)
  (declare (ignore action))
  (list (1- state) (1+ state)))

(defmethod initial-state ((walk random-walk))
  (floor (walk-n walk) 2))

(defmethod terminal-state? ((walk random-walk) state)
  (or (> state (walk-n walk))
      (< state 1)))

(defmethod state-features ((walk random-walk) state)
  ;; this appears to be very important...
  (loop for i from 1 to (walk-n walk)
     collect (if (= i state) 1 0))
  
  )


