(in-package :cl-rl)

(defun rms (lst)
  (sqrt (loop for x in lst sum (* x x))))

(defun random-walk-totd-lambda-solver (n walks simulations report-file &key (alpha 0.4) (td-lambda 0.8))
  "n-sized random walk"
  (let* ((problem (make-instance 'cl-rl-problems::random-walk :n n))
         (all-states (loop for j from 1 to n collect j))
         (rms-tab (make-hash-table :test #'eq)))
    (loop repeat simulations do
         (let ((agent (make-totd-lambda-agent problem :alpha alpha :gamma 1.0 :td-lambda td-lambda)))
           (loop for i from 1 to walks do
                (run-mdp-with-agent problem agent :max-steps nil)
                (incf (gethash i rms-tab 0)
                      (rms (mapcar
                            (lambda (s)
                              ;; (format t "v(~A) = ~A~%"
                              ;;         s
                              ;;         (funcall (agent-value-function agent) s t))
                              
                              (- (funcall (agent-value-function agent) s)
                                 (/ s (1+ n))))
                            all-states)))
                ;; (format t "theta = ~A~%" (agent-theta agent))
                )))
    (with-open-file (s report-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (loop for i from 1 to walks do
           (format s " ~A~%" (float (/ (gethash i rms-tab) simulations)))))))

