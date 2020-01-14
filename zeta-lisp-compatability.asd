


(asdf:defsystem :zeta-lisp-compatability
  :depends-on (misc-extensions cl-utilities hemlock.base)
  :components
  ((:file package)
   (:file primitive)
   (:file system)
   (:file zeta-lisp-compatability)
   (:file zmacs-macros)
   (:file zwei-defs)
   (:file zwei-comtab)
   (:file zwei-macros)
   (:file z-to-c-translator/translate)))
