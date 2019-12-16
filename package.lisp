
(defpackage :system
  (:nicknames sys)
  (:use cl)
  (:export printing-random-object))

(defpackage :zeta-lisp-compatability
  (:nicknames zlisp)
  (:use cl)
  (:export with-stack-list))
