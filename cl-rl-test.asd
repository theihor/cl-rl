(asdf:defsystem cl-rl-test
  :depends-on (#:cl-rl)
  :components ((:module "test"
                        :serial t
                        :components ())))
