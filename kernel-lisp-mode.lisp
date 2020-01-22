

(in-package :si)



;;; 152

(defun zetalisp-on-p (&optional globally)
  "returns true if the current lisp mode is :zetalisp and returns false otherwise.
   if globally is non-nil, the global bindings, instead of the local
   bindings, are checked."
  (if globally
      (eq (symeval-globally '*lisp-mode*) :zetalisp)
      (eq *lisp-mode* :zetalisp)))
