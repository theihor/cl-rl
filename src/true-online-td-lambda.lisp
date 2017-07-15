(in-package :cl-rl)

(defclass true-online-td-lambda-agent (gpi-agent)
  ((theta :initarg :theta
          :initform nil
          :type list
          :accessor agent-theta
          :documentation "State feature weights")
   (eligibility-trace :initarg :eligibility-trace
                      :initform nil
                      :type list
                      :accessor agent-e
                      :documentation "Eligibility trace list")
   (alpha :initarg :alpha
          :initform 0.4
          :type real
          :accessor agent-alpha
          :documentation "Learning rate")
   (gamma :initarg :gamma
          :initform 0.99
          :type real
          :reader agent-gamma
          :documentation "Discount factor")
   (agent-lambda :initarg :lambda
            :initform 0.8
            :type real
            :reader agent-lambda
            :documentation "Trace decay parameter")
   (last-value :initform 0 
               :type real
               :accessor agent-last-value)
   )
  (:documentation "Agent that performs GPI using True Online TD(lambda) for policy evaluation"))

(defun copy-totd-agent (a)
  (copy-instance
   a
   :theta (copy-list (agent-theta a))
   :eligibility-trace (copy-list (agent-e a))))

(defun make-totd-lambda-agent (mdp &key (alpha 0.4) (gamma 0.99) (td-lambda 0.8))
  (let ((n (length (state-features mdp (initial-state mdp)))))
    (make-instance 'true-online-td-lambda-agent
                   :theta (make-list n :initial-element 0)
                   :eligibility-trace (make-list n :initial-element 0)
                   :alpha alpha
                   :gamma gamma
                   :lambda td-lambda
                   :policy (lambda (state)
                             (let ((as (possible-actions mdp state)))
                               (nth (random (length as)) as))))))

(defun dot-list (lst1 lst2)
  (loop
     for x in lst1
     for y in lst2
     sum (* x y)))

(defmethod agent-update-value-function ((agent true-online-td-lambda-agent)
                                        (mdp markov-decision-process)
                                        reward action from-state to-state)
  (with-slots (theta alpha gamma agent-lambda last-value) agent
    (let* ((from-fi (state-features mdp from-state))
           (to-fi (state-features mdp to-state))
           (from-v (dot-list from-fi theta))
           (to-v (dot-list to-fi theta)))
      
      ;; (format t "from-fi = ~A~%" (car from-fi))
      ;; (format t "to-fi   = ~A~%" (car to-fi))
      ;; (format t "last-v  = ~A~%" last-value)
      ;; (format t "from-v  = ~A; to-v = ~A~%" from-v to-v)
      ;; (format t "theta   = ~A~%" (car theta))
      ;; (format t "e       = ~A~%" (car (agent-e agent)))
   
       (let ((e-fi (dot-list (agent-e agent) from-fi)))
         ;; (format t "e-fi = ~A~%" e-fi)
        (setf (agent-e agent)
              (mapcar
               (lambda (e fi)
                 (+ (* gamma agent-lambda e)
                    (* alpha fi (- 1 (* gamma agent-lambda e-fi)))))
               (agent-e agent)
               from-fi))
        (let ((delta (+ reward (- (* gamma to-v) last-value))))
          ;; (format t "delta = ~A~%" delta)
          (setf theta (mapcar
                       (lambda (th e fi)
                         (+ th
                            (* e delta)
                            (* alpha fi (- last-value from-v))))
                       theta (agent-e agent) from-fi))
          (setf last-value to-v)))))
  (lambda (state)
    ;; (format t "theta in V: ~A~%" (agent-theta agent))
    ;; (format t "fi in V:    ~A~%" (state-features mdp state))
    (dot-list (agent-theta agent)
              (state-features mdp state))))

(defmethod agent-update-policy :after ((agent true-online-td-lambda-agent) (mdp markov-decision-process)
                                       reward action from-state to-state)
  (when (terminal-state? mdp to-state) 
    (setf (agent-last-value agent) 0)
    (setf (agent-e agent) (make-list (length (state-features mdp to-state))
                                     :initial-element 0))))
