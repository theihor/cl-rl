(in-package :cl-rl)

(defun k-armed-bandit-epsilon-greedy-solver (n k epsilon steps report-file)
  "n of k-armed bandits"
  (let* ((problems (loop repeat n collect (cl-rl-problems::make-random-kab k)))
         (agents (loop repeat n collect (make-instance 'epsilon-greedy-agent
                                                       :epsilon epsilon)))
         (optimal-actions (mapcar (lambda (kab)
                                    (let ((best-a (first (alexandria:hash-table-keys
                                                          (cl-rl-problems::kab-reward-rvs kab))))
                                          (best-mean (mean (first (alexandria:hash-table-values
                                                                   (cl-rl-problems::kab-reward-rvs kab))))))
                                      (maphash
                                       (lambda (a rv)
                                         (when (> (mean rv) best-mean)
                                           (setf best-mean (mean rv))
                                           (setf best-a a)))
                                       (cl-rl-problems::kab-reward-rvs kab))
                                      best-a))
                                  problems))
         (total-rewards (make-hash-table :test #'eq))
         (optimal-action-n (make-hash-table :test #'eq)))
    (loop
       for kab in problems
       for optimal-a in optimal-actions
       for agent in agents
       do (run-mdp-with-agent
           kab agent
           :max-steps steps
           :report-func (lambda (i agent mdp total-reward reward action state next-state)
                          (declare (ignore agent mdp total-reward state next-state))
                          (incf (gethash i total-rewards 0)
                                reward)
                          (when (action-equal optimal-a action)
                            (incf (gethash i optimal-action-n 0))))))
    (with-open-file (s report-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (loop for i from 1 to steps do
           (format s " ~A~%" (float (/ (gethash i total-rewards) n)))
           ;; (format s " ~A~%" (float (/ (gethash i optimal-action-n) n)))
           ))
    )
  )

;; (time (K-ARMED-BANDIT-EPSILON-GREEDY-SOLVER 2000 10 0.00 10000 "./e000.mat"))
;; (time (K-ARMED-BANDIT-EPSILON-GREEDY-SOLVER 2000 10 0.01 10000 "./e001.mat"))
;; (time (K-ARMED-BANDIT-EPSILON-GREEDY-SOLVER 2000 10 0.10 10000 "./e010.mat"))

