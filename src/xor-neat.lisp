(in-package :neat)

(defclass xor-nn-genotype (neat-genotype) ())
(defclass xor-nn-population (neat-population)
  ())

(defun xor-nn-genotype-generator (min-w max-w)
  (lambda (index)
    (declare (ignore index))
    (let* ((nodes (list (make-instance 'nn::node :weights nil :input? t :id 1)
                        (make-instance 'nn::node :weights nil :input? t :id 2)
                        (make-instance 'nn::node :weights nil :output? t :id 3
                                       :bias (cl-rl::random-float :from min-w :to max-w))))
           (links (list (make-instance 'nn::link :from (nth 0 nodes) :to (nth 2 nodes)
                                       :index 4 :w (cl-rl::random-float :from min-w :to max-w))
                        (make-instance 'nn::link :from (nth 1 nodes) :to (nth 2 nodes)
                                       :index 5 :w (cl-rl::random-float :from min-w :to max-w))))
           (net (make-instance 'nn::network :links links)))
      (make-instance 'xor-nn-genotype :network net))))

(defun xor-fitness (g)
  (with-slots (network) g 
    (let ((f (- 4.0 (+ (abs (- 0 (car (nn::prop network '(0 0)))))
                       (abs (- 1 (car (nn::prop network '(0 1)))))
                       (abs (- 1 (car (nn::prop network '(1 0)))))
                       (abs (- 0 (car (nn::prop network '(1 1)))))))))
      (* f f))))

(defmethod fitness ((g xor-nn-genotype))
  (xor-fitness g))

(defun best-genotype1 (pop)
  (maximum (genotype-list pop)
           :comparator (lambda (g1 g2)
                         (compare-number (xor-fitness g1)
                                         (xor-fitness g2)))))

(defmethod terminate? ((pop xor-nn-population))
  (< (abs (- 16 (xor-fitness (best-genotype1 pop))))
     0.32))

(defun solve-xor (&key population-size timeout max-iteration)
  (let ((*elitism-rate* 0.15) 
        (*tournament-rate* 0.6)
        (*initial-mutation-depth* 0.1)
        (*initial-mutation-width* 0.6)
        (*mutation-decrease-rate* :constant) 
        (*species* (make-hash-table :test #'eq))
        (*genetic-distance-threshold* 5.0)
        (*genetic-distance-c1* 1.0)
        (*genetic-distance-c2* 1.0)
        (*genetic-distance-c3* 0.4)
        (*new-node-probability* 0.01)
        (*new-link-probability* 0.02)
        (*min-weight* -20.0)
        (*max-weight* +20.0)
        (*interspecies-mating-rate* 0.001)
        (*stagnation-threshold* 20)
        (*enable-disabled-link-chance* 0.2)
        (*mutation-only-rate* 0.1)) 
    (let* ((pop (initialize-population
                 xor-nn-population
                 (xor-nn-genotype-generator *min-weight* *max-weight*)
                 population-size))
           (*species* (compute-species pop))
           (the-best (xor-fitness (best-genotype1 pop))))
      (setf pop
            (evolution pop
                       :timeout timeout
                       :max-iteration max-iteration
                       :step-func (lambda (population i elapsed-time)
                                    (let* ((best (best-genotype1 population))
                                           (value (xor-fitness best)))
                                      (if (< the-best value)
                                          (progn (setf the-best value)
                                                 (format t "~%~A [~A s]: ~,2F (~A pops)~%"
                                                         i (float elapsed-time) the-best
                                                         (length (genotype-list pop))))
                                          (format t ".")))
                                    ;; (compute-species pop)
                                    )))
      (format t "~%BEST VALUE = ~A~%" the-best)
      (values (best-genotype1 pop) pop))))
