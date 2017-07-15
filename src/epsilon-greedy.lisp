(in-package :cl-rl)

(defclass epsilon-greedy-agent (rl-agent)
  ((epsilon :initform 0
            :initarg :epsilon
            :type real
            :reader agent-epsilon)
   (action->value-estimate :initform (make-hash-table :test #'eq)
                           :initarg :action->value-estimate
                           :type hash-table
                           :accessor agent-action->q)
   (action->n :initform (make-hash-table :test #'eq)
              :initarg :action-value-times
              :type hash-table
              :accessor agent-action->n))
  (:documentation "Agent with tabular epsilon-greedy policy"))

(defgeneric agent-best-estimated-action (agent possible-actions))

(defmethod agent-best-estimated-action ((agent epsilon-greedy-agent) possible-actions)
  (let* ((a->q (agent-action->q agent))
         (best-a (car possible-actions))
         (best-q (gethash best-a a->q 0)))
    
    (loop for a in (cdr possible-actions) do
         (let ((q (gethash a a->q 0)))
           (when (> q best-q)
             (progn (setf best-a a)
                    (setf best-q q)))))
    best-a))

(defmethod agent-action ((agent epsilon-greedy-agent) (mdp markov-decision-process) state)
  (let* ((actions (possible-actions mdp state)))
    (when (null actions)
      (error "No possible actions. Terminal state reached?"))
    (if (probability-check (agent-epsilon agent))
        (nth (random (length actions)) actions)
        (agent-best-estimated-action agent actions))))

(defmethod agent-update-policy ((agent epsilon-greedy-agent) (mdp markov-decision-process)
                                reward action from-state to-state)
  (declare (ignore mdp from-state to-state))
  (incf (gethash action (agent-action->n agent) 0))
  (let ((q (gethash action (agent-action->q agent) 0))
        (n (gethash action (agent-action->n agent))))
    (setf (gethash action (agent-action->q agent))
          (+ q (/ (- reward q) n)))))

