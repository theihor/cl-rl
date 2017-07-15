(defpackage :cl-rl
  (:use #:common-lisp ;; #:cl-random
        #:anaphora)
  (:export
   #:markov-decision-process
   #:possible-actions
   #:perform-action
   #:policy
   #:next-states
   #:initial-state
   #:terminal-state?
   #:state-features

   #:run-mdp
   #:execute-policy

   #:probability-check
   ))


