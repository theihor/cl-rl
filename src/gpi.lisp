;; Generalized policy iteration
;; 'Reinforcement Learning: An Introduction' by R. S. Sutton, A. G. Barto, p. 93

(in-package :cl-rl)

(defclass gpi-agent (rl-agent)
  ((policy :initarg :policy
           :accessor agent-policy
           :type function
           :documentation "Mapping from states to actions")
   (value-function :initarg :value-function
                   :accessor agent-value-function
                   :type function
                   :documentation "Value function is an estimate of how good is given state-action pair
                                   considering estimated cummulative future reward.
                                   V(π,s) - the value of being in state s under a policy π
                                               here π is implicit part of an agent
                                   so this is function of kind (foo state) -> value"))
  
  (:documentation "Agent with generalized policy iteration"))

(defmethod agent-action ((agent gpi-agent) (mdp markov-decision-process) state) 
  (funcall (agent-policy agent) state))

(defgeneric agent-update-value-function (agent mdp reward action from-state to-state)
  (:documentation "Step of updating `value-function'
                   Must be specified for specific policy evaluation algorithm"))

(defmethod agent-update-policy ((agent gpi-agent) (mdp markov-decision-process)
                                reward action from-state to-state)
  "Default policy improvement is simple greedy around value-function"
  (let ((new-vf (agent-update-value-function agent mdp reward action from-state to-state)))
    (setf (agent-value-function agent) new-vf)
    (setf (agent-policy agent)
          (lambda (state)
            (let* ((actions (possible-actions mdp state))
                   (vf (agent-value-function agent)))
              (labels ((%best-v (a)
                         (apply #'max (mapcar (lambda (srp)
                                                (destructuring-bind (state reward probability) srp
                                                  (declare (ignore reward))
                                                  (* probability (funcall vf state))))
                                              (estimate-action mdp state a)))))
                (let* ((best-a (first actions))
                       (best-v (%best-v best-a)))
                  (if nil ;; (< (random 1.0) 1e-4)
                      (first (random-take 1 actions))
                      (progn
                        ;; (format t "V(~A) = ~A; V(~A) = ~A~%"
                        ;;         (first actions)
                        ;;         (funcall vf (perform-action mdp state (first actions)))
                        ;;         (second actions)
                        ;;         (funcall vf (perform-action mdp state (second actions))))
                        (loop for a in (cdr actions) do
                             (let ((v (%best-v a)))
                               (when (> v best-v)
                                 (setf best-v v)
                                 (setf best-a a))))
                        best-a)))))))))

