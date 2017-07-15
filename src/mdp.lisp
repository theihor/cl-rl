(in-package :cl-rl)

(defclass markov-decision-process () ())

(defgeneric possible-actions (mdp state)
  (:documentation "Returns list of possible actions in given `state'"))

(defgeneric perform-action (mdp state action)
  (:documentation "Executes specified `action', returns (values new-state reward)"))

(defgeneric estimate-action (mdp state action)
  (:documentation "Estimates specified `action', returns list of lists (new-state reward probability)
    This is needed to distinguish between actually changing the state and estimating the change.
    Useful for cases when it is not known exactly new-state properties until action is performed."))

(defmethod estimate-action ((mdp markov-decision-process) state action)
  "By default just return result of `perform-action', since this should work for many problems"
  (multiple-value-bind (new-state reward)
      (perform-action mdp state action)
    (list (list new-state reward 1.0))))

(defgeneric policy (mdp state)
  (:documentation "Policy maps states to actions"))

(defgeneric next-states (mdp state action)
  (:documentation "Returns list of possible states after performing `action' in `state'"))

(defgeneric initial-state (mdp))
(defgeneric terminal-state? (mdp state))

(defgeneric state-equal (s1 s2)
  (:documentation "States comparison"))

(defmethod state-equal (s1 s2)
  "Default comparison"
  (eq s1 s2))

(defgeneric state-features (mdp state)
  (:documentation "Returns list of values that describe state"))

(defmethod state-features (mdp state)
  "Return `state' itself by default"
  (declare (ignore mdp))
  (list state))

(defgeneric action-equal (a1 a2)
  (:documentation "Actions comparison"))

(defmethod action-equal (a1 a2)
  "Default comparison"
  (eq a1 a2))

(defun run-mdp (mdp initial-state actions &key (discount-factor 0.99))
  (let ((i 0)
        (total-reward 0))
    (reduce
     (lambda (state action)
       (multiple-value-bind (state reward) (perform-action mdp state action)
         (incf total-reward (* (expt discount-factor i) reward))
         (incf i)
         state))
     actions
     :initial-value initial-state)
    total-reward))

(defun execute-fixed-policy (mdp initial-state policy &key stop-condition (max-steps 1000) (discount-factor 0.99))
  (let ((current-state initial-state)
        (total-reward 0))
    (loop for i from 0 to max-steps do
         (let ((action (funcall policy mdp current-state)))
           (when (and stop-condition
                      (funcall stop-condition mdp current-state action))
             (return-from execute-fixed-policy (values current-state total-reward)))
           (multiple-value-bind (state reward) (perform-action mdp current-state action)
             (incf total-reward (* (expt discount-factor i) reward))
             (incf i)
             (setf current-state state))))
    (values current-state total-reward)))

