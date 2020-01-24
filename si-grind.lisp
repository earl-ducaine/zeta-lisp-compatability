

(in-package :si)


;;; line 63


(defmacro gtyo (ch &optional notify)
  `(progn
     ,(and notify `(and grind-notify-fun
			(neq grind-io #'grind-count-io)
			(funcall grind-notify-fun ,ch ,notify nil)))
     (write ,ch :stream grind-io)))


;;; line 82


(defun gstring (string &optional notify)
  (when (and notify grind-notify-fun (neq grind-io #'grind-count-io))
    (funcall grind-notify-fun string notify nil))
  (write string grind-io))
