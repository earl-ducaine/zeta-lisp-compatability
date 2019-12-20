


(defpackage :primitive-zlisp
  (:use cl)
  (:shadow defstruct)
  (:import-from new-let nlet)
  (:export art-32b
	   memq
	   defstruct))

(defpackage :system
  (:nicknames sys)
  (:use cl primitive-zlisp)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:export printing-random-object
	   art-32b
	   memq
	   *clipping-rectangle-right-edge*
	   *clipping-rectangle-left-edge*
	   *clipping-rectangle-top-edge*
	   *clipping-rectangle-bottom-edge*
	   mouse-x
	   mouse-y))

(defpackage :ticl
  (:use cl))

(defpackage :time
  (:use cl primitive-zlisp)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:export get-universal-time))

(defpackage :zeta-lisp-compatability
  (:nicknames zlisp)
  (:use cl)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:export with-stack-list
	   defstruct))
