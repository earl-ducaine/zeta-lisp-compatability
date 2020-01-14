
(in-package :system)

(defvar *clipping-rectangle-right-edge*)
(defvar *clipping-rectangle-left-edge*)
(defvar *clipping-rectangle-top-edge*)
(defvar *clipping-rectangle-bottom-edge*)

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

;; todo ed -- this is really needs to be translated into an entry into
;; the pretty printing table. But that works on object types not on
;; functions (and their return values).
(defmacro defprint (function-name way)
  "Defines a Way for PPrint to print a call to the function named by
   Function-Name.  See ??? for details."
  (if (listp way)
      `(setf (get ',function-name ',(car way)) ',(cadr way))
      `(setf (get ',function-name 'specially-grind) ',way)))
