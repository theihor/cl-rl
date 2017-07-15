(in-package :cl-rl-problems)

(defclass k-armed-bandit (markov-decision-process)
  ((k :initform 1
      :initarg :k
      :type integer
      :reader kab-k) 
   (reward-rvs :initform (make-hash-table :test #'eq)
               :initarg :reward-rvs
               :type hash-table
               :reader kab-reward-rvs
               :documentation "Contains random variables of actual reward for each action")))

(defun make-random-kab (k &key (variance 1.0) (mean 0))
  "Generates random k-armed-bandit with `k' arms
     each arm gives reward from normal distribution
     where its mean is also from normal distribution N(`mean', `variance')
   See 'Reinforcement Learning: An Introduction' by R. S. Sutton, A. G. Barto, p. 30
  "
  (let ((rv (r-normal mean variance))
        (reward-rvs (make-hash-table :test #'eq)))
    (loop for i from 1 to k do
         (setf (gethash i reward-rvs)
               (r-normal (draw rv) variance)))
    (make-instance 'k-armed-bandit
                   :k k 
                   :reward-rvs reward-rvs)))

(defmethod initial-state ((kab k-armed-bandit))
  t)

(defmethod possible-actions ((kab k-armed-bandit) state)
  (declare (ignore state))
  (loop for i from 1 to (kab-k kab) collect i))

(defmethod perform-action ((kab k-armed-bandit) state action)
  (let ((rv (gethash action (kab-reward-rvs kab))))
    (values state (if rv (draw rv) 0))))

(defmethod next-states ((kab k-armed-bandit) state action)
  (declare (ignore action))
  (list state))



