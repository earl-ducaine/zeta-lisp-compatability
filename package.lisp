

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
  (:nicknames sys si zlc)
  (:use cl primitive-zlisp)
  (:shadowing-import-from primitive-zlisp defstruct)
  (:export *clipping-rectangle-bottom-edge*
	   *clipping-rectangle-left-edge*
	   *clipping-rectangle-right-edge*
	   *clipping-rectangle-top-edge*
	   *common-lisp-on-p*
	   *common-lisp-symbol-substitutions*
	   *default-pathname-defaults*
	   *lisp-mode*
	   *prindepth*
	   *read-discard-font-changes*
	   *reader-symbol-substitutions*
	   *zetalisp-symbol-substitutions*
	   *zetalisp-symbol-substitutions*
	   /
	   ar-1
	   ar-1-force
	   aref
	   art-32b
	   assoc
	   atan
	   character
	   close
	   condition-case-throw
	   defprint
	   defstruct
	   delete
	   eval
	   evalhook
	   every
	   find-system-named
	   float
	   format
	   get-source-file-name
	   intersection
	   io-stream-p
	   lambda
	   listp
	   make-hash-table
	   make-pp-obj
	   map
	   member
	   memq
	   merge-pathname-type
	   mouse-x
	   mouse-y
	   named-lambda
	   named-subst
	   nintersection
	   nlistp
	   nunion
	   package
	   pp-obj-callish
	   pp-obj-length
	   pp-obj-object
	   pp-obj-type
	   pp-objify
	   pp-objify-comment
	   pprint-handler
	   printing-random-object
	   rassoc
	   read
	   read-discard-font-changes
	   read-from-string
	   readtable
	   rem
	   remove
	   some
	   string
	   string-capitalize-words
	   subst
	   system-files
	   system-symbolic-name
	   terpri
	   union
	   xr-list-so-far
	   zetalisp-on-p))

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
  (:export applyhook
	   ar-1
	   ar-1-force
	   case
	   cond
	   declare
	   defmacro
	   defmethod
	   defun
	   do*
	   dolist
	   evalhook
	   flet
	   labels
	   lambda
	   let
	   let*
	   macro
	   macrolet
	   named-lambda
	   named-subst
	   nlistp
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
  (:use cl)
  (:export
   defun-compatibility
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

(defpackage :tv
  (:use cl)
  (:export font-char-width))
