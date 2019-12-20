





(asdf:defsystem :g7c
    :depends-on (misc-extensions)
    :components
    ((:file package)
     (:file defs)
     (:file genera-7-font-reader-patch)
     (:file genera-7-font-zmacs-patch)
     (:file flavors)
     (:file misc)
     (:file style-checkers)
     (:file processes)
     (:file reader-patch)))


;; (asdf:defsystem :g7c
;;    (:name "g7c-compatibility")
;;    (:short-name "g7c")
;;    (:not-in-disk-label)
;;    (:pathname-default "sys:public.g7-compatibility;")
;;    (:module packages "packages")
;;    (:module defs     "defs")
;;    (:module reader-patch "genera-7-font-reader-patch")
;;    (:module main     ("genera-7-font-zmacs-patch" "flavors" "misc" "style-checkers" "processes"))
;;    (:compile-load    packages)
;;    (:compile-load    reader-patch
;; 		     (:fasload packages)
;; 		     (:fasload packages))
;;    (:compile-load    defs    (:fasload packages reader-patch)
;; 		             (:fasload packages reader-patch))
;;    (:compile-load    main    (:fasload packages reader-patch defs)
;; 		             (:fasload packages reader-patch defs)))
