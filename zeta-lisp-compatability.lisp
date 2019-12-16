

(in-package :zlisp)

(defmacro with-stack-list ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST (var exp1 ... expN) body) Equivalent to
  ;; (LET ((var (MAPCAR #'EVAL '(exp1 ... expN)))) body) except that
  ;; the list produced by MAPCAR resides on the stack and therefore
  ;; DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list ,@elements)))
     (declare (type cons ,var)
	      (dynamic-extent ,var))
     ,@body))
