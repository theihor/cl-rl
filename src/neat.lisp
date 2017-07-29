(defpackage :neat
  (:use #:common-lisp #:cl-rl #:nn #:anaphora :cl-gena/generic-defs :cl-gena/fancy-tools :mgl-mat))

(in-package :neat)

(defclass neat-genotype (genotype)
  ((network :reader genotype-network
            :initarg :network)
   (disabled-links :accessor disabled-links
                   :initform nil
                   :initarg :disabled)))

(defclass neat-population (population)
  ((species-tab :accessor species-tab
                :initform (make-hash-table :test #'eq))
   (g0->f :accessor population-g0->f
          :initform (make-hash-table :test #'eq)
          :documentation "g0 -> (cons best-f k)")))

(defparameter *innovation-number* 10)
(defparameter *innovations* (make-hash-table :test #'equal)
  "(from to what) -> inn-number")

(defun innovation-number (from to what)
  "what is :link or :node"
  (let ((key (list from to what)))
    (aif (gethash key *innovations*)
         it
         (setf (gethash key *innovations*)
               (incf *innovation-number*)))))

(defun mutate-new-link (g &key (w nil) (trys 10))
  (with-slots (network) g
    (let ( ;; (layers (nn::network-layers network))
          
          (pair nil)) 

      (labels ((%out-links (node)
                 (loop :for link :in (append (disabled-links g)
                                             (network-links network))
                    :when (eq node (from-node link))
                    :collect link))
               (%has-path? (n1 n2)
                 (let ((out-links (%out-links n1)))
                   (or (some (lambda (link) (= (nn::id (nn::to-node link))
                                          (nn::id n2)))
                             out-links)
                       (some (lambda (link) (%has-path? (nn::to-node link) n2)) out-links)))))
        (loop :for i :from 0 :until (or pair (>= i trys)) :do
           (let* ((from-node (cl-rl::random-choose (remove-if #'nn::output? (nn::network-nodes network))))
                  (to-node (cl-rl::random-choose
                            (remove from-node (remove-if #'nn::input? (nn::network-nodes network))
                                    :test #'eq))))
             ;; (format t "~A -> ~A~%" from-node to-node)
             ;; (format t "links: ~A~%" (nn::network-links network))
             (unless (or (some (lambda (link) (and (= (nn::id (nn::from-node link))
                                                 (nn::id from-node))
                                              (= (nn::id (nn::to-node link))
                                                 (nn::id to-node))))
                               (%out-links from-node))
                         (%has-path? to-node from-node))
               (setf pair (cons from-node to-node)))
             (incf i))
           
           ;; (multiple-value-bind (layer left-ls right-ls)
           ;;     (cl-rl::split-at (random (- (length layers) 1)) layers)
           ;;   (declare (ignore left-ls))
           ;;   (let* ((from-node (cl-rl::random-choose layer))
           ;;          (out-links (nn::node-out-links network from-node))
           ;;          (lst nil))
           ;;     (loop :for l :in right-ls :do
           ;;        (loop :for n :in l
           ;;           :unless (or (eq n from-node)
           ;;                       (nn::input? n)
           ;;                       (some (lambda (link) (eq n (to-node link))) out-links))
           ;;           :do (pushnew n lst :test #'eq)))
           ;;     (if lst
           ;;         (let ((to-node (cl-rl::random-choose lst)))
           ;;           (setf pair (cons from-node to-node)))
           ;;         (incf i))))
           ))

      (unless pair (return-from mutate-new-link g))
      
      (destructuring-bind (from-node . to-node) pair
        (let ((new-link (make-instance 'nn::link
                                       :from from-node
                                       :to to-node
                                       :index (innovation-number (nn::id from-node) (nn::id to-node) :link)
                                       :w w)))
          (nn::update-network-by-links network (cons new-link (network-links network)))))))
  g)

(defun mutate-new-node (g &key (w-in nil) (w-out nil))
  ;; (format t "mutating new node!~%")
  (with-slots (network) g
    ;; (format t "~A~%" (network-links network))
    (let* ((link (cl-rl::random-choose (network-links network)))
           (new-node (make-instance 'nn::node
                                    :id (innovation-number
                                         (nn::id (from-node link))
                                         (nn::id (to-node link)) 
                                         :node)
                                    :bias (random-weight)))
           (in-link (make-instance 'nn::link
                                   :from (from-node link)
                                   :to new-node
                                   :index (innovation-number (nn::id (from-node link))
                                                             (nn::id new-node)
                                                             :link)
                                   :w (or w-in 1.0d0)))
           (out-link (make-instance 'nn::link
                                    :from new-node
                                    :to (to-node link)
                                    :index (innovation-number (nn::id new-node)
                                                              (nn::id (to-node link))
                                                              :link)
                                    :w (or w-out (link-w link)))))
      (push (disabled-links g) link)
      (nn::update-network-by-links network
                                   (append (list in-link out-link)
                                           (remove link (network-links network)))) 
      ;; (format t "~A~%" (node-weights new-node))
      (check-weird g)
      ))
  g)

(defparameter *max-weight* +10.0)
(defparameter *min-weight* -10.0)

(defun w+ (x y)
  (let ((z (+ x y)))
    (cond ((< z *min-weight*) *min-weight*)
          ((> z *max-weight*) *max-weight*)
          (t z))))

(defun random-weight ()
  (cl-rl::random-float :from *min-weight* :to *max-weight*))

(defun mutate-change-weight (g)
  (labels ((%tweak (w)
             (w+ w (* (if (probability-check 0.5) -1.0 1.0)
                      *mutation-depth*
                      (random-weight)))))
    (with-slots (network) g
      (let* ((lst (append (nn::network-nodes network)
                          (network-links network)))
             (k (* *mutation-width* (length lst)))
             (k (if (< k 1) 1 (round k)))
             (lst (cl-rl::random-take k lst)))
        (loop :for link-or-node :in lst :do
           (if (< (random 1.0) 0.9)
               (etypecase link-or-node
                 (nn::link (setf (nn::link-w link-or-node) (%tweak (nn::link-w link-or-node))))
                 (nn::node (setf (nn::bias link-or-node) (%tweak (nn::bias link-or-node)))))
               (etypecase link-or-node
                 (nn::link (setf (nn::link-w link-or-node) (random-weight)))
                 (nn::node (setf (nn::bias link-or-node) (random-weight))))))
        
        (nn::update-network-by-links network (network-links network))))))

(defparameter *new-link-probability* 2e-2)
(defparameter *new-node-probability* 1e-2)

(defun check-weird (g)
  (when (loop :for link :in (nn::network-links (genotype-network g)) :do
           (when (some (lambda (l)
                         (and (= (nn::id (nn::from-node link))
                                 (nn::id (nn::from-node l)))
                              (= (nn::id (nn::to-node link))
                                 (nn::id (nn::to-node l)))))
                       (remove link (nn::network-links (genotype-network g))))
             (format t "links: ~A~%"
                     (nn::network-links (genotype-network g)))
             (error "Something weird happened!~%")))))

(defmethod mutate ((g neat-genotype))
  
  (let ((r (random 1.0)))
    (cond ((< r *new-link-probability*)
           (mutate-new-link g :w (random-weight)))
          ((< r (+ *new-link-probability*
                   *new-node-probability*))
           (mutate-new-node g))
          (t (mutate-change-weight g)))
   
    g))

(defparameter *genetic-distance-c1* 1.0)
(defparameter *genetic-distance-c2* 1.0)
(defparameter *genetic-distance-c3* 0.4)
(defparameter *genetic-distance-threshold* 3.0)

(defun genetic-distance (g1 g2)
  (let ((res
         (let* ((links1 (sort (copy-list (append (network-links (genotype-network g1))
                                                 (disabled-links g1)))
                              #'< :key #'nn::index))
                (links2 (sort (copy-list (append (network-links (genotype-network g2))
                                                 (disabled-links g2)))
                              #'< :key #'nn::index))
                (n (max (length links1) (length links2)))
                (n (if (< n 10) 1 n))
                (matching-n 0))
           (labels ((%count (ls1 ls2 &key (d 0) (w 0))
                      "Returns (values D E)"
                      (if (and ls1 ls2)
                          (cond ((= (nn::index (first ls1))
                                    (nn::index (first ls2)))
                                 (incf matching-n)
                                 (%count (cdr ls1) (cdr ls2)
                                         :d d :w (+ w (abs (- (nn::link-w (first ls1))
                                                              (nn::link-w (first ls2)))))))
                                ((> (nn::index (first ls1))
                                    (nn::index (first ls2)))
                                 (%count ls1 (cdr ls2) :d (1+ d) :w w))
                                (t ;; (< (index (first ls1))
                                 ;;    (index (first ls2)))
                                 (%count (cdr ls1) ls2 :d (1+ d) :w w)))
                          (values d (length (or ls1 ls2)) w))))
             (multiple-value-bind (d e w) (%count links1 links2)
               (+ (/ (* *genetic-distance-c1* e) n)
                  (/ (* *genetic-distance-c2* d) n)
                  (/ (* *genetic-distance-c3* (/ w (if (= matching-n 0) 1 matching-n))) n)))))))
    ;; (when (or (> (length (nn::network-links (genotype-network g1))) 2)
    ;;           (> (length (nn::network-links (genotype-network g2))) 2))
    ;;   (format t "distance = ~A~%" res))
    res))

(defparameter *species* (make-hash-table :test #'eq) "g -> species size")

;; (defparameter *distances* nil)

(defun compute-species (population &key (clear nil))
  "`known' is list of genotypes that DO NOT belong to `population'.
   It is used to classify `population' by species represented by `known'"
  (let ((lst (genotype-list population))
        (g0->g-list (make-hash-table :test #'eq))
        (known (alexandria:hash-table-keys (species-tab population))))
    ;; (format t "computing species with ~A known~%" (length known))
    (when clear (clrhash *species*))
    (loop :for g :in lst :do
       (aif (find-if (lambda (g0)
                       (let ((d (genetic-distance g g0)))
                         ;; (push d *distances*)
                         ;; (format t "d = ~A (~A)~%" d (< d *genetic-distance-threshold*))
                         (< d *genetic-distance-threshold*)))
                     known)
            (push g (gethash it g0->g-list))
            (progn (push g known)
                   (push g (gethash g g0->g-list))))
       ;; (setf lst (remove g lst))
       )
   
    (maphash (lambda (g0 g-list)
               (declare (ignore g0))
               (let ((size (length g-list))) 
                 (loop :for g :in g-list :do
                    (setf (gethash g *species*) size))))
             g0->g-list)
    (setf (species-tab population) g0->g-list)
    *species*))

(defmethod fitness :around ((g neat-genotype))
  (let ((res
         (aif (gethash g cl-gena/generic-defs::*fitness-cache*)
              it
              (let* ((f (call-next-method))
                     (f (/ f (gethash g *species*))))
                (setf (gethash g cl-gena/generic-defs::*fitness-cache*) f)))))
    ;; (when (> (length (nn::network-links (genotype-network g))) 2)
    ;;   (format t "~A = ~A~%" (nn::network-links (genotype-network g)) res ))
    res))

(defparameter *enable-disabled-link-chance* 0.25)

(defun crossover-aux (g1 g2)
  (let* ((links1 (sort (copy-list (append (network-links (genotype-network g1))
                                          (disabled-links g1)))
                       #'< :key #'nn::index))
         (links2 (sort (copy-list (append (network-links (genotype-network g2))
                                          (disabled-links g2)))
                       #'< :key #'nn::index))
         (f1 (fitness g1))
         (f2 (fitness g2)))
    ;; (format t "links1: ~A~%" links1)
    ;; (format t "links2: ~A~%" links2)
    (labels ((%collect (ls1 ls2 &key acc)
               (if (and ls1 ls2)
                   (cond ((= (nn::index (first ls1))
                             (nn::index (first ls2)))
                          (%collect (cdr ls1) (cdr ls2)
                                    :acc (cons (cl-rl::random-choose (list (first ls1) (first ls2)))
                                               acc)))
                         ((> (nn::index (first ls1))
                             (nn::index (first ls2)))
                          (if (>= f2 f1)
                              (%collect ls1 (cdr ls2) :acc (cons (first ls2) acc))
                              (%collect ls1 (cdr ls2) :acc acc)))
                         (t ;; (< (index (first ls1))
                          ;;    (index (first ls2)))
                          (if (> f1 f2)
                              (%collect (cdr ls1) ls2 :acc (cons (first ls1) acc))
                              (%collect (cdr ls1) ls2 :acc acc))))
                   (if (>= f2 f1)
                       (append (reverse acc) ls2)
                       (append (reverse acc) ls1)))))
      (let* ((links (%collect links1 links2))
             nodes
             new-links)
        (loop :for link :in links :do
           (when (or (member link (disabled-links g1) :test #'eq)
                     (member link (disabled-links g2) :test #'eq))
             (unless (< (random 1.0) *enable-disabled-link-chance*)
               (setf links (remove link links :test #'eq)))))
        ;; (when (< (length links) 2)
        ;;   (format t "[~A] p1 = ~A~%" f1 (mapcar #'nn::index links1))
        ;;   (format t "[~A] p2 = ~A~%" f2 (mapcar #'nn::index links2))
        ;;   (format t "ls = ~A~%" (mapcar #'nn::index links)))
        (loop :for link :in links :do 
           (unless (some (lambda (n) (= (nn::id n)
                                   (nn::id (nn::from-node link))))
                         nodes)
             (push (nn::copy-node (nn::from-node link)) nodes))
           (unless (some (lambda (n) (= (nn::id n)
                                   (nn::id (nn::to-node link))))
                         nodes)
             (push (nn::copy-node (nn::to-node link)) nodes))
           (push (copy-instance link
                                :from (find-if (lambda (node) (= (nn::id node)
                                                            (nn::id (nn::from-node link))))
                                               nodes)
                                :to (find-if (lambda (node) (= (nn::id node)
                                                          (nn::id (nn::to-node link))))
                                             nodes))
                 new-links))
        ;; (format  t "links = ~A~%~%" new-links)
        (setf new-links (sort new-links #'< :key #'nn::index))
        
        (let ((res
               (if (check-recursive-links new-links)
                   (progn (warn "Found recursive links in an offspring")
                          (return-from crossover-aux nil))
                   (copy-instance g1 :network (make-instance 'nn::network :links new-links)))))
          (when (some
                 (lambda (link)
                   (or (null (nn::to-node link))
                       (null (nn::from-node link))
                       (not (member (nn::to-node link)
                                    (nn::network-nodes (genotype-network res))
                                    :test #'eq))
                       (not (member (nn::from-node link)
                                    (nn::network-nodes (genotype-network res))
                                    :test #'eq))))
                 (nn::network-links (genotype-network res)))
            (error "Invalid individual! ~A~%" (nn::network-links (genotype-network res))))
          res)))))

(defmethod crossover ((g1 neat-genotype) (g2 neat-genotype))
  (awhen (crossover-aux g1 g2)
         (list it)))

(defun mutated-network (n &key (new-links 10) (new-nodes 5))
  (let ((g (make-instance 'neat::neat-genotype :network (copy-network n))))
    (loop :repeat new-nodes :do (neat::mutate-new-node g))
    (loop :repeat new-links :do (neat::mutate-new-link g))
    g))


(defun copy-individual (i)
  (copy-instance i :network (nn::copy-network (genotype-network i))))

(defparameter *interspecies-mating-rate* 0.001)
(defparameter *stagnation-threshold* 15)
(defparameter *mutation-only-rate* 0.2)

(defmethod evolve ((pop neat-population))
  "Performs one iteration of evolution, spawning new population"
  (let ((new-generation nil)
        (ns nil)
        ;; (*innovations* (make-hash-table :test #'equal))
        )


    
    
    (maphash
     (lambda (g0 g-list)
       (declare (ignore g0))
       ;; (when (> (length (nn::network-links (genotype-network g0))) 2) 
       ;;   (format t "species ~A~%" (nn::network-links (genotype-network g0)))
       ;;   (loop :for g :in g-list :do
       ;;      (format t "  ~A = ~A~%" (nn::network-links (genotype-network g)) (fitness g)))
       ;;   (format t "~%"))


       
       
       (let ((n (length g-list)))
         (if (and (> n 3)
                  (> n (* 0.01 (size pop))))
             (multiple-value-bind (elite champions) (select-from-list g-list)
               (let* ((parents (append elite champions))
                      (prob (/ (/ (size pop) 3)
                               (let ((n (length parents)))
                                 (if (> n 2) (/ (* n (1- n)) 2) n))))
                      (prob (if (> prob 1) 1.0 prob))
                      (children (loop
                                   :for p1 :in parents
                                   :for i :from 0
                                   :append (multiple-value-bind (_1 _2 rest) (cl-rl::split-at i parents)
                                             (declare (ignore _1 _2))
                                             (loop for p2 in rest
                                                unless (eq p1 p2)
                                                when (probability-check prob) 
                                                append (labels ((%mate (p1 p2)
                                                                  (if (probability-check *mutation-only-rate*)
                                                                      (list (mutate (copy-individual p1)))
                                                                      (mapcar #'mutate (crossover p1 p2)))
                                                                  ))
                                                         (append (%mate p1 p2) (%mate p2 p1))))))) 
                      (new-g-list (append elite children)))
                 (push new-g-list new-generation)
                 (push (length g-list) ns)))
             (progn (push g-list new-generation)
                    (push n ns)))))
     (species-tab pop))

    ;; interspecies mating
    (multiple-value-bind (n frac)
        (floor (* *interspecies-mating-rate* (size pop)))
      (let* ((n (if (< (random 1.0) frac) (1+ n) n))
             (g0s (alexandria:hash-table-keys (species-tab pop)))
             (children (loop :repeat n :append
                          (destructuring-bind (g0-1 g0-2)
                              (cl-rl::random-take 2 g0s)
                            (let ((p1 (cl-rl::random-choose (gethash g0-1 (species-tab pop))))
                                  (p2 (cl-rl::random-choose (gethash g0-2 (species-tab pop)))))
                              ;; (format t "~%p1 = ~A~%" (nn::network-links (genotype-network p1)))
                              ;; (format t "p2 = ~A~%" (nn::network-links (genotype-network p2))) 
                              (mapcar #'mutate (crossover p1 p2)))))))
        (push children new-generation)
        (push (length children) ns)
        ;; (format t "interspecies offspring: ~A; " (length children))
        ))

    ;; mutation only offspring
    ;; (multiple-value-bind (n frac)
    ;;     (floor (* *mutation-only-rate* (size pop)))
    ;;   (let* ((n (if (< (random 1.0) frac) (1+ n) n))
             
    ;;          (children (mapcar #'mutate (cl-rl::random-take n (genotype-list pop)))))
    ;;     (push children new-generation)
    ;;     (push (length children) ns)
    ;;     ;; (format t "interspecies offspring: ~A; " (length children))
    ;;     ))

    
    ;; (format t "number of mutated individuals: ~A~%"
    ;;         (loop :for g :in (reduce #'append new-generation) 
    ;;            :when (> (length (nn::network-links (genotype-network g))) 2)
    ;;            :count 1))

    
    
    (let ((p (copy-instance pop :genotype-list (reduce #'append new-generation))))
      (compute-species p :clear t)

      ;; (format t "~A -> ~A species, "
      ;;         (hash-table-count (species-tab pop))
      ;;         (hash-table-count (species-tab p)))
      ;; (format t "~A individuals, " (length (genotype-list pop)))
      ;; (format t "~A offsprings~%" (length (genotype-list p)))
      
      
      
      (let ((g0->max-children (make-hash-table :test #'eq))
            (total-fitness 0))

        (maphash (lambda (g0 g-list)
                   (let* ((fs (mapcar #'fitness g-list))
                          (f (apply #'+ fs))
                          (max-f (maximum fs)))

                     (aif (gethash g0 (population-g0->f p))
                          (if (> max-f (car it))
                              (setf (gethash g0 (population-g0->f p))
                                    (cons max-f 0))
                              (setf (gethash g0 (population-g0->f p))
                                    (cons (car it) (1+ (cdr it)))))
                          (setf (gethash g0 (population-g0->f p))
                                (cons max-f 0)))

                     (if (> (cdr (gethash g0 (population-g0->f p)))
                            *stagnation-threshold*)
                         (progn
                           ;; (format t "stagnant species found~%")
                           (setf (gethash g0 g0->max-children) 0))
                         (setf (gethash g0 g0->max-children) f))
                     
                     (incf total-fitness (gethash g0 g0->max-children))))
                 (species-tab p))

        (maphash (lambda (g0 g-list)
                   (declare (ignore g-list))
                   (let* ((f (gethash g0 g0->max-children))
                          (n (round (* (/ f total-fitness) (size pop))))) 
                     ;; (when (> (length (nn::network-links (genotype-network g0))) 2) 
                     ;;   (format t "[n = ~A] species ~A~%" n (nn::network-links (genotype-network g0)))
                     ;;   (loop :for g :in g-list :do
                     ;;      (format t "  ~A = ~A~%" (nn::network-links (genotype-network g)) (fitness g)))
                     ;;   (format t "~%"))
                     
                     (setf (gethash g0 g0->max-children) n)))
                 (species-tab p))

        (let ((new-species-tab (make-hash-table :test #'eq)))
          
          (maphash
           (lambda (g0 g-list)
             (awhen (gethash g0 g0->max-children)
                    ;; (format t "size = ~A; n = ~A~%" (length g-list) it) 
                    (if (> it 0)
                        (setf (gethash g0 new-species-tab)
                              (take-n-max it g-list :comparator #'fitness-comparator))
                        (remhash g0 new-species-tab))))
           (species-tab p))

          (let* ((new-list (reduce #'append (alexandria:hash-table-values new-species-tab)))
                 (g0s (alexandria:hash-table-keys new-species-tab))
                 (total-lack (- (size pop) (length new-list)))
                 (pool (set-difference (genotype-list p) new-list :test #'eq))
                 (lucky (cl-rl::random-take total-lack pool))
                 ;; (new-species-tab (make-hash-table :test #'eq))
                 )

            
            ;; (compute-species p)
            
            (labels ((%speciate (g)
                       (let ((g0 (loop :for g0 :in g0s
                                    :when (< (genetic-distance g0 g) *genetic-distance-threshold*)
                                    :return g0
                                    :finally (return g))))
                         (push g (gethash g0 new-species-tab)))))
              (mapc #'%speciate lucky))

            (setf (genotype-list p) (append lucky new-list))
            (setf (species-tab p) new-species-tab)
            
            ;; (maphash
            ;;  (lambda (g0 g-list)
            ;;    (when (cdr g-list)
            ;;      (let* ((n 0)
            ;;             (ds (loop for g in g-list :unless (eq g g0)
            ;;                    :collect (progn (incf n)
            ;;                                    (genetic-distance g g0)))))
            ;;        (loop :for d in ds
            ;;           :when (>= d *genetic-distance-threshold*)
            ;;           :do (format t "found big d in one sp~%")))))
            ;;  (species-tab p))
            
            ;; (labels ((%push (g)
            ;;            (unless (member g new-list :test #'eq)
            ;;              (push g new-list)
            ;;              t)))
            ;;   (loop :while (and pool (< (length new-list) (size pop)))
            ;;      :do
            ;;      (format t "~A ~A ~A~%" (length new-list) lack (length pool))
            ;;      (let ((lst (cl-rl::random-take lack pool))) 
            ;;        (loop :for g :in lst
            ;;           :do (progn (pushnew g new-list :test #'eq)
            ;;                      (when (%push )
            ;;                        (decf lack))))
            ;;        (setf pool (set-difference pool lst :test #'eq)))))
            
            
            
            ;; (maphash (lambda (g0 g-list)
            ;;            (when (> (length (nn::network-links (genotype-network g0))) 2) 
            ;;              (format t "species ~A~%" (nn::network-links (genotype-network g0)))
            ;;              (loop :for g :in g-list :do
            ;;                 (format t "  ~A = ~A~%" (nn::network-links (genotype-network g)) (fitness g)))
            ;;              (format t "~%")))
            ;;          new-species-tab)
            
            ;; (setf (species-tab p) new-species-tab)
            ;; (setf (genotype-list p) new-list)
            )))
      
      
      p)))

