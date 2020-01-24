

(in-package :si)


;;; 138


(defsubst common-lisp-on-p (&optional globally)
  "Returns true if the current lisp mode is :common-lisp and returns
   false otherwise. If globally is non-nil, the global bindings,
   instead of the local bindings, are checked."
  (if globally
      (eq (symeval-globally '*lisp-mode*) :common-lisp)
      (eq *lisp-mode* :common-lisp)))
