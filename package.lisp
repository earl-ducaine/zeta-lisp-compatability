


(defpackage :primitive-zlisp
  (:use cl)
  (:shadow defstruct)
  (:import-from new-let nlet)
  (:export art-32b
	   memq
	   defstruct
	   ferror
	   condition-name-handled-p
	   fquery))

(defpackage :system
  (:nicknames sys si)
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
	   mouse-y
	   *prindepth*
	   read-discard-font-changes
	   *lisp-mode*
	   *reader-symbol-substitutions*
	   *common-lisp-symbol-substitutions*
	   io-stream-p
	   condition-case-throw))

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

(defpackage :zwei
  (:use cl primitive-zlisp)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:export skip-over-blank-lines-and-comments
	   defcom
	   with-undo-save
	   forward-sexp
	   point))

(defpackage :compiler
  (:export
   warn))

(defpackage :transl
  (:use cl)
  (:shadow warning))

(defpackage :conditions
  (:nicknames eh)
  (:use cl primitive-zlisp)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:export condition-name-handled-p))

(defpackage :cleh
  (:use cl)
  (:export restart))
