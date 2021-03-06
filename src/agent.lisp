(in-package :cl-rl)

(defclass rl-agent () ())

(defgeneric agent-action (agent mdp state)
  (:documentation "Returns action that `agent' would perform for given `mdp' and `state'"))

(defgeneric agent-update-policy (agent mdp reward action from-state to-state)
  (:documentation "Here agent's policy is updated based on recieved experience"))

(defun run-mdp-with-agent (mdp agent
                           &key (max-steps 100) (discount-factor 0.99)
                             report-func)
  (let ((i 0)
        (total-reward 0)
        (state (initial-state mdp)))
    (loop until (or (and max-steps (>= i max-steps))
                    (terminal-state? mdp state))
       do (let ((action (agent-action agent mdp state)))
            (multiple-value-bind (next-state reward) (perform-action mdp state action)
              (incf total-reward (* (expt discount-factor i) reward))
              (incf i)
              (agent-update-policy agent mdp reward action state next-state)
              (when report-func
                (funcall report-func i agent mdp total-reward reward action state next-state))
              (setf state next-state)))) 
    total-reward))

