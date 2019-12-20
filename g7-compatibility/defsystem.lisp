;;; -*- Mode:Common-Lisp; Package:(USER); Fonts:(MEDFNT HL12B HL12I MEDFNT MEDFNTB); Patch-file:T; Base:10 -*-

;;; "Genera" and "Symbolics" are trademarks of Symbolics, Inc.
;;; "Explorer" is a trademark of Texas Instruments.

(DEFSYSTEM G7C
   (:name "G7C-Compatibility")
   (:short-name "G7C")
   (:not-in-disk-label)
   (:pathname-default "SYS:PUBLIC.G7-COMPATIBILITY;")
   (:module packages "packages")
   (:module defs     "defs")
   (:module reader-patch "genera-7-font-reader-patch")
   (:module main     ("genera-7-font-zmacs-patch" "flavors" "misc" "style-checkers" "processes"))
   (:compile-load    packages)
   (:compile-load    reader-patch
		     (:fasload packages)
		     (:fasload packages))
   (:compile-load    defs    (:fasload packages reader-patch)
		             (:fasload packages reader-patch))
   (:compile-load    main    (:fasload packages reader-patch defs)
		             (:fasload packages reader-patch defs)))
