


(asdf:defsystem :zeta-lisp-compatability
  :depends-on (misc-extensions alexandria)
  :components
  ((:file package)
   (:file primitive)
   (:file system)
   (:file zeta-lisp-compatability)
   (:file z-to-c-translator/translate)))
