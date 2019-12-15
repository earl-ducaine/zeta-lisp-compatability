

(defpackage :zeta-lisp-compatability
  (:nickname zlisp)
  (:shadow ignore-errors handler-case restart-case handler-bind
	   invoke-restart find-restart with-simple-restart
	   parse-error *default-server-path*)
   (:use cl))
