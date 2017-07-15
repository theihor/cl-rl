(defpackage :neural-network
  (:nicknames :nn)
  (:use #:common-lisp #:cl-rl #:mgl-mat #:anaphora)
  (:import-from :cl-gena/fancy-tools
                :copy-instance)
  (:export #:node-weights
           #:network-links
           #:from-node
           #:to-node
           #:link-w
           #:link
           #:node
           #:copy-network
           #:check-recursive-links))

(in-package :nn)

(defclass node ()
  ((w :initarg :weights
      :accessor node-weights
      :initform nil)
   (input? :initarg :input?
           :reader input?
           :initform nil)
   (output? :initarg :output?
            :reader output?
            :initform nil)
   (id :initarg :id
       :reader id)
   (bias :initarg :bias
         :accessor bias
         :initform 1.0d0)))

(defmethod print-object ((n node) s)
  (format s "~A" (id n)))

(defclass link ()
  ((from :initarg :from
         :reader from-node)
   (to :initarg :to
       :reader to-node)
   (index :initarg :index
          :reader index)
   (w :accessor link-w
      :initarg :w
      :initform 1.0d0)))

(defmethod print-object ((l link) s)
  (format s "~A->~A" (from-node l) (to-node l)))

(defclass network ()
  ((links :initarg :links
          :accessor network-links) 
   (layers :accessor network-layers)
   (nodes :accessor network-nodes)))

(defun copy-node (node)
  (copy-instance node :weights (awhen (node-weights node) (copy-mat it))))

(defun copy-network (n)
  (let* ((nodes (mapcar #'copy-node (network-nodes n)))
         (links (mapcar (lambda (link)
                          (copy-instance link
                                         :from (find-if (lambda (node) (= (id node)
                                                                     (id (from-node link))))
                                                        nodes)
                                         :to (find-if (lambda (node) (= (id node)
                                                                   (id (to-node link))))
                                                      nodes)))
                        (network-links n)))
         (new-n (make-instance 'network :links links)))
    ;; (update-network-by-links new-n (network-links new-n))
    ;; (format t "l: ~A~%" (network-links new-n))
    new-n))

(defun log-sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))

(defgeneric transfer (node inputs))

(defmethod transfer ((n node) inputs)
  (log-sigmoid (dot (node-weights n) inputs)))

;; (with-cuda* ( ) (time (let ((m1 (make-mat '(200 200)))
;;                                      (m2 (make-mat '(200 200))))
;;                                  (loop for i from 1 repeat 4000 do
;;                                       (fill! i m1)
;;                                       (fill! (* 1.13 i) m2)
;;                                       (geem! 1.0 m1 m2 1.0 (make-mat '(200 200)))))))

(defparameter *nodes*
  (list
   t
   (make-instance 'node :input? t :id 1) ; 1
   (make-instance 'node :input? t :id 2) ; 2
   (make-instance 'node :input? t :id 3) ; 3
   (make-instance 'node :id 4) ; 4
   (make-instance 'node :id 5) ; 5 
   (make-instance 'node :output? t :id 6) ; 6
   (make-instance 'node :output? t :id 7) ; 7
   
   (make-instance 'node :id 8)
   ))

(defparameter *links*
  (reverse (list (make-instance 'link :from (nth 1 *nodes*) :to (nth 4 *nodes*) :index 0 :w 1)
                 ;; (make-instance 'link :from (nth 2 *nodes*) :to (nth 4 *nodes*) :index 1)
                 (make-instance 'link :from (nth 3 *nodes*) :to (nth 5 *nodes*) :index 2 :w 1)
                 (make-instance 'link :from (nth 4 *nodes*) :to (nth 6 *nodes*) :index 3 :w 1)
                 (make-instance 'link :from (nth 4 *nodes*) :to (nth 7 *nodes*) :index 4 :w 1)
                 (make-instance 'link :from (nth 5 *nodes*) :to (nth 7 *nodes*) :index 5 :w 1)

                 (make-instance 'link :from (nth 2 *nodes*) :to (nth 8 *nodes*) :index 6 :w 1)
                 (make-instance 'link :from (nth 8 *nodes*) :to (nth 4 *nodes*) :index 7 :w 1)
                 (make-instance 'link :from (nth 1 *nodes*) :to (nth 6 *nodes*) :index 8 :w 1)
                 )))

;; (defparameter *n* (make-instance 'network :links *links*))

(defparameter *test-nodes1*
  (list t
        (make-instance 'node :input? t :id 1)
        (make-instance 'node :id 2)
        (make-instance 'node :id 3)
        (make-instance 'node :output? t :id 4)))

(defparameter *test-links1*
  (list (make-instance 'link :from (nth 1 *test-nodes1*) :to (nth 2 *test-nodes1*) :index 0 :w 1)
        (make-instance 'link :from (nth 2 *test-nodes1*) :to (nth 3 *test-nodes1*) :index 1 :w 2)

        ;; (make-instance 'link :from (nth 3 *test-nodes1*) :to (nth 2 *test-nodes1*) :index 4 :w 2)
        
        (make-instance 'link :from (nth 2 *test-nodes1*) :to (nth 4 *test-nodes1*) :index 2 :w 2)
        (make-instance 'link :from (nth 3 *test-nodes1*) :to (nth 4 *test-nodes1*) :index 3 :w 3)))

;; (defparameter *test-n1* (make-instance 'network :links *test-links1*))

(defun check-recursive-links (links)
  ;; (format t "links: ~A~%" links)
  (labels ((%check (node &optional visited)
             ;; (format t "~A ~A~%" node visited)
             (let ((out-links (remove-if-not (lambda (link) (eq node (nn::from-node link)))
                                             links)))
               (when (some (lambda (link) (member (nn::to-node link) visited :test #'eq)) out-links)
                 (return-from check-recursive-links t))
               (loop :for n :in (mapcar #'nn::to-node out-links) :do
                  (%check n (cons node visited))))))
    (let* ((start-nodes (remove-if-not #'nn::input? (mapcar #'nn:from-node links)))) 
      (loop :for n :in start-nodes :do (%check n)))))

(defun update-network-by-links (network links) 
  (let ((layers (get-layers links))
        (nodes nil)
        (processed (make-hash-table :test #'eq)))

    (loop :for l :in layers :do
       (loop :for n :in l :do
          (pushnew n nodes :test #'eq)))
    (setf (network-nodes network) nodes)
    
    (loop :for (l1 l2 . rest) on layers
       :when (and l1 l2)
       :do (loop :for n :in l2
              :unless (input? n)
              :unless (gethash n processed)
              :do (let* ((in-links (remove-if-not (lambda (link) (eq n (to-node link))) links))
                         (ws (cons (bias n)
                                   (mapcar (lambda (in-n) (aif (find-if (lambda (link) (eq in-n (from-node link)))
                                                                   in-links)
                                                          (link-w it)
                                                          0))
                                           l1))))
                    (setf (node-weights n)
                          (make-mat (list (1+ (length l1))) :initial-contents ws))
                    (setf (gethash n processed) t))))
    
    (setf (network-links network) links)
    (setf (network-layers network) layers)))

(defun update-network-by-nodes (network) 
  (let ((layers (network-layers network))
        (links (network-links network))
        (processed (make-hash-table :test #'eq)))
    (loop :for (l1 l2 . rest) on layers
       :when (and l1 l2)
       :do (loop :for n :in l2
              :unless (input? n)
              :unless (gethash n processed)
              :do (let* ((in-links (remove-if-not (lambda (link) (eq n (to-node link))) links))
                         (ws (node-weights n)))
                    (setf (bias n) (mref ws 0))
                    (loop :for link :in in-links :do
                       (setf (link-w link)
                             (mref ws (1+ (position (from-node link) l1)))))
                    (setf (gethash n processed) t))))))

(defun node-in-links (network node)
  (loop :for link :in (network-links network)
     :when (eq node (to-node link))
     :collect link))

(defun node-out-links (network node)
  (loop :for link :in (network-links network)
     :when (eq node (from-node link))
     :collect link))

(defun get-layers (links)
  (when (check-recursive-links links)
    (error "Recursive link found in get-layers"))
  (let* ((last-layer (remove-duplicates (loop :for link :in links
                                           :when (output? (to-node link))
                                           :collect (to-node link))))
         (layers (list last-layer)))
    (loop :until (every #'input? last-layer) :do
       (let ((layer (remove-duplicates
                     (mapcar #'from-node
                             (remove-if-not (lambda (link) (member (to-node link) last-layer))
                                            links)))))
         (push layer layers)
         (setf last-layer layer)))
    (mapcar (lambda (l) (sort l #'< :key #'id)) layers)))

(defgeneric net-w-matrices (network))

(defmethod net-w-matrices ((n network))
  (let ((processed (make-hash-table :test #'eq)))
    (labels ((%matrix (layer)
               (let ((layer (remove-if (lambda (n) (or (input? n)
                                                  (gethash n processed)))
                                       layer)))
                 (when layer
                   ;; (stack! 0 (mapcar #'node-weights layer)
                   ;;         (make-mat (list (length layer) (mat-size (node-weights (first layer))))))
                   (map-concat #'copy!
                               (loop :for n :in layer 
                                  :collect (progn (setf (gethash n processed) t)
                                                  (node-weights n)))
                               (make-mat (list (length layer)
                                               (mat-size (node-weights (first layer))))))))))
      (mapcar #'%matrix (network-layers n)))))

(defmethod initialize-instance :after ((n network) &key links)
  (update-network-by-links n links))

(defun propagate (net input)
  "Here input is list at first"
  (let ((outs (make-hash-table :test #'eq)))
    (labels ((%out (n) (gethash n outs))
             (%inputs (l) 
               (loop :for n :in l
                  :when (input? n) :collect (nth (1- (id n)) input)
                  :when (%out n) :collect (%out n))))
      (let* ((layers (network-layers net))
             (w-matrices (net-w-matrices net))
             (a (%inputs (first layers)))
             (a (make-mat (list 1 (1+ (length a)))
                          :initial-contents (list (cons 1 a))))
             (as (list a)))
        (loop
           :for l :in (cdr layers)
           :for w :in (cdr w-matrices) :do
           ;; (format t "a = ~A; w = ~A~%" a w)
           (let* ((a1 (.logistic! (m* a w :transpose-b? t)))
                  (i (%inputs l))
                  (ns (remove-if (lambda (n) (or (nn::input? n) (%out n))) l)))

             (loop
                :for n :in ns
                :for i :from 0 
                :do
                ;; (format t "set ~A to ~A~%" n (mref a1 0 i))
                (setf (gethash n outs)
                      (mref a1 0 i)))
             
             (setf a (map-concat #'copy!
                                 (list (make-mat (1+ (length i))
                                                 :initial-contents (cons 1 i))
                                       a1)
                                 (make-mat (list 1 (+ 1 (length i) (mat-size a1))))))
             (push a as)))
        (values ;; (cdr (loop :for x :across (mat-to-array (copy-row a 0)) :collect x))
         a as)
        ))))

(defun prop (net inp)
  (multiple-value-bind (a _) (propagate net inp)
    (declare (ignore _))
    (cdr (loop :for x :across (mat-to-array (copy-row a 0)) :collect x))))

(defparameter *backpropagation-alpha* 0.9d0)

(defun backpropagate (net input output)
  (labels ( ;; (%inputs (l)
           ;;   (remove nil
           ;;           (loop :for n :in l :collect
           ;;              (nth (1- (id n)) input))))
           (%slice (d n)
             (if (= n (second (mat-dimensions d)))
                 d
                 (let ((d (transpose d)))
                   (map-concat
                    #'copy! (list (stack 0
                                         (reverse (loop :for i :downfrom (1- (first (mat-dimensions d)))
                                                     :repeat n
                                                     :collect (copy-row d i)))))
                    (make-mat (list 1 n)))))
             ))
    
    (let ((layers (network-layers net))
          (w-matrices (net-w-matrices net)))
      (multiple-value-bind (a as) (propagate net input) 
        (let* ((ys (mat-to-array a))
               (ys (loop :for i :from 1 :below (second (array-dimensions ys)) :collect (aref ys 0 i)))
               (out-delta (mapcar #'- output ys)) 
               (d (make-mat (list 1 (length out-delta)) :initial-contents (list out-delta)))
               (deltas nil))
          ;; computing errors (deltas)
          (loop
             :for a :in (cdr as)
             :for w :in (reverse (cdr w-matrices)) :do
             ;; (format t "slice = ~A~%" (%slice d (first (mat-dimensions w))))
             ;; (format t "d = ~A~%" d)
             ;; (format t "w = ~A~%" w)
             ;; (format t "a = ~A~%" a)
             (let* ((sliced-d (%slice d (first (mat-dimensions w))))
                    ;; (sliced-a (%slice a (first (mat-dimensions w))))
                    ;; (foo (progn (format t "sliced-a = ~A~%" sliced-a)
                    ;;             (format t "sliced-d = ~A~%" sliced-d)))
                    (next-d (m* sliced-d w)
                      
                      ))
               
               (push sliced-d deltas)
               
               ;; (format t "x = ~A~%" (.*! next-d (.*! a (axpy! -1.0 a
               ;;                                                (make-mat (mat-dimensions a)
               ;;                                                          :initial-element 1.0)))))
               (setf next-d (.*! next-d (.*! a (axpy! -1.0 a
                                                      (make-mat (mat-dimensions a)
                                                                :initial-element 1.0)))))
               (setf d next-d)))
          
          ;; adjusting weights
          (let ((processed (make-hash-table :test #'eq)))
            (loop
               :for a :in (reverse (cdr as))
               :for d :in deltas
               :for l :in (cdr layers) 
               :do (let ((delta (m* (transpose d) a)))
                     ;; (format t "d = ~A~%" d)
                     ;; (format t "a = ~A~%" a)
                     ;; (format t "d*a = ~A~%" delta) 
                     (loop
                        :for i :from 0 :below (first (mat-dimensions delta))
                        :for node :in (remove-if (lambda (n) (or (input? n)
                                                            (gethash n processed)))
                                                 l) 
                        :do
                        (let ((w (node-weights node)))
                          (setf (node-weights node)
                                (map-mats-into (make-mat (first (mat-dimensions w)))
                                               (lambda (x y) (if (= x 0) 0.0d0 (+ x (* *backpropagation-alpha* y))))
                                               w
                                               (copy-row delta i)))
                          (setf (gethash node processed) t)
                          ;; (format t "new-w: ~A~%" (node-weights node))
                          )
                        )
                     
                     )))

          ;; (format t "~%~%ws = ~A~%" w-matrices)
          ;; (format t "deltas = ~A~%" deltas)
          ;; (break)
          )))
    ))

(defun backpropagate-many (net input-output-pairs &key (times 1))
  (loop :repeat times :do
     (loop :for p :in input-output-pairs :do
        (destructuring-bind (input . output) p
          (backpropagate net input output))))
  (update-network-by-nodes net))

(defun init-weights (n f)
  (make-mat (1+ n) :initial-contents (loop :repeat (1+ n) :collect (funcall f))))

(defparameter *xor-nodes* 
  (let* ((input-n 2)
         (output-n 1)
         (e (/ (sqrt 6) (sqrt (+ input-n output-n)))))
    (flet ((r () (let ((x (- (* 2 e (random 1.0)) e)))
                   (if (< (abs x) 1e-3)
                       1e-3
                       x))))
      (list
       t
       (make-instance 'node :weights nil :input? t :id 1) ; 1
       (make-instance 'node :weights nil :input? t :id 2) ; 2
       (make-instance 'node :weights (init-weights 2 #'r)  :id 3)
       ;; (make-instance 'node :weights (init-weights 2 #'r) :id 4)
       (make-instance 'node :weights (init-weights 3 #'r) :output? t :id 4)
       )
      )))

(defparameter *xor-links*
  (reverse (list (make-instance 'link :from (nth 1 *xor-nodes*) :to (nth 3 *xor-nodes*) :index 0) 
                 (make-instance 'link :from (nth 1 *xor-nodes*) :to (nth 4 *xor-nodes*) :index 1)
                 (make-instance 'link :from (nth 2 *xor-nodes*) :to (nth 3 *xor-nodes*) :index 2)
                 (make-instance 'link :from (nth 2 *xor-nodes*) :to (nth 4 *xor-nodes*) :index 3) 
                 (make-instance 'link :from (nth 3 *xor-nodes*) :to (nth 4 *xor-nodes*) :index 4)
                 ;; (make-instance 'link :from (nth 4 *xor-nodes*) :to (nth 5 *xor-nodes*) :index 5)
                 )))

(defparameter *xor-n* (make-instance 'network :links *xor-links*))

(defun network->dot (net s)
  (with-slots (links) net
    (format s "digraph g {~%")
    (loop :for link :in links :do
       (format s "    ~A -> ~A [label=\"~1,2F\"];~%"
               ;; "    ~A -> ~A;~%"
               (id (from-node link))
               (id (to-node link))
               (link-w link)
               ))
    
    (format s "}~%")))
