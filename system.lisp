
(in-package :system)

(defmacro printing-random-object ((object stream . keywords)
				  &body body)
  (let ((typep (memq :typep keywords))
	(no-pointer (memq :no-pointer keywords)))
    `(progn ,(if typep
		 `(format ,stream "#<~A " (type-of ,object))
		 `(write-string "#<" ,stream))
	    ,@body
	    ,(if no-pointer
		 `(write-char #\> ,stream)
		 `(format ,stream " ~X>" (%pointer ,object)))
	    nil)))
