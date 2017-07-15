(asdf:defsystem :cl-rl
  :licence ""
  :version "0.0.1"
  :author "Ihor Solodrai"
  :mailto "the.ihor@gmail.com"
  :homepage ""
  :description "CL-RL is a library of reinforcement learning algorithms such
  as Temporal Difference Learning."
  :depends-on (;; #:cl-random
               #:alexandria #:anaphora #:mgl-mat #:cl-gena)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package") 
                                     (:file "mdp")
                                     (:file "random")
                                     (:file "agent" :depends-on ("mdp")) 
                                     (:file "epsilon-greedy" :depends-on ("agent"))
                                     (:file "gpi" :depends-on ("agent"))
                                     (:file "true-online-td-lambda" :depends-on ("gpi"))
                                     (:file "nn" :depends-on ("random"))
                                     (:file "neat" :depends-on ("nn"))
                                     (:file "xor-neat" :depends-on ("neat"))))
               (:module "problems"
                        :serial t
                        :components ((:file "package")
                                     ;; (:file "k-armed-bandit")
                                     (:file "random-walk"))
                        :depends-on ("src"))
               ))

;; (defmethod asdf:perform ((o asdf:test-op)
;;                          (c (eql (asdf:find-system '#:cl-rl))))
;;   (asdf:oos 'asdf:load-op '#:cl-rl-test)
;;   (funcall (intern (symbol-name '#:test) (find-package '#:cl-rl))))

