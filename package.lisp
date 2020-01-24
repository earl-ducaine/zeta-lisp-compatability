

(defpackage :asdf-helper
  (:use cl)
  (:export system-files))

(defpackage :global
  (:use cl)
  (:export format))

(defpackage :primitive-zlisp
  (:use cl)
  (:shadow defstruct)
  (:import-from new-let nlet)
  (:export art-32b
	   condition-name-handled-p
	   defprop
	   defstruct
	   ferror
	   fquery
	   maclisp-defun
	   memq
	   neq
	   string-capitalize-words))

(defpackage :system
  (:nicknames sys si)
  (:use cl primitive-zlisp)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:export *clipping-rectangle-bottom-edge*
	   *clipping-rectangle-left-edge*
	   *clipping-rectangle-right-edge*
	   *clipping-rectangle-top-edge*
	   *common-lisp-on-p*
	   *common-lisp-symbol-substitutions*
	   *lisp-mode*
	   *prindepth*
	   *read-discard-font-changes*
	   *reader-symbol-substitutions*
	   *zetalisp-symbol-substitutions*
	   art-32b
	   condition-case-throw
	   defprint
	   find-system-named
	   get-source-file-name
	   io-stream-p
	   make-pp-obj
	   memq
	   merge-pathname-type
	   mouse-x
	   mouse-y
	   pp-obj-callish
	   pp-obj-length
	   pp-obj-object
	   pp-obj-type
	   pp-objify
	   pp-objify-comment
	   pprint-handler
	   printing-random-object
	   read-discard-font-changes
	   string-capitalize-words
	   system-files
	   system-symbolic-name
	   zetalisp-on-p
	   xr-list-so-far))

(defpackage :ticl
  (:use cl)
  (:shadow case
	   cond
	   declare
	   defmacro
	   defmethod
	   defun
	   do*
	   dolist
	   flet
	   labels
	   lambda
	   let
	   let*
	   macro
	   macrolet
	   named-lambda
	   prog
	   prog*
	   quote
	   setf
	   setq
	   the
	   trace)
  (:export case
	   cond
	   declare
	   defmacro
	   defmethod
	   defun
	   do*
	   dolist
	   flet
	   labels
	   lambda
	   let
	   let*
	   macro
	   macrolet
	   named-lambda
	   prog
	   prog*
	   quote
	   setf
	   setq
	   the
	   trace))

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
  (:use cl primitive-zlisp global)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:export barf
	   defcom
	   forward-sexp
	   point
	   set-comtab
	   skip-over-blank-lines-and-comments
	   with-undo-save))

(defpackage :compiler
  (:export
   warn))

(defpackage :transl
  (:use cl zwei primitive-zlisp)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:shadow warning)
  (:import-from si zetalisp-on-p)
  (:export translate-file
	   translate-system
	   condition-case-throw
	   tr-comm))

(defpackage :conditions
  (:nicknames eh)
  (:use cl primitive-zlisp)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:export condition-name-handled-p
	   condition-case-throw))

(defpackage :cleh
  (:use cl)
  (:export restart))


(defpackage :file-system
  (:use cl)
  (:nicknames fs)
  (:export merge-pathname-defaults
	   read-attribute-list
	   file-attribute-bindings))
