


(asdf:defsystem :zeta-lisp-compatability
  :depends-on (misc-extensions alexandria hemlock.base)
  :components
  ((:file package)
   (:file primitive)
   (:file system)
   (:file kernel-lisp-mode)
   (:file kernel-reader)
   (:file kernel-variable-definitions)
   (:file si-pprint)
   (:file si-variable-definitions)
   (:file si-grind)
   (:file compiler-zetalisp)
   (:file zeta-lisp-compatability)
   (:file zwei-comtab)
   (:file zwei-macros)
   (:file zwei-meth)
   (:file zwei-displa)
   (:file asdf-helper)
   (:file z-to-c-translator/translate)))
