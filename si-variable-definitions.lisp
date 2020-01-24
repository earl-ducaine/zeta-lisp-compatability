

(in-package :si)


;; PAD 4/2/87 Remove substitution of decode-float.
;; They should be identical for release3.[SPR 4506] (CR:PHD).
(defparameter *zetalisp-symbol-substitutions*
	  '((cl:/ . zlc:/)
	    (cl:*default-pathname-defaults* . zlc:*default-pathname-defaults*)
	    (ticl:applyhook . zlc:applyhook)	; jlm 4/24/89
	    (ticl:ar-1 . zlc:ar-1)
	    (ticl:ar-1-force . zlc:ar-1-force)
	    (cl:aref . zlc:aref)
	    (cl:assoc . zlc:assoc)
	    (cl:atan . zlc:atan)
	    (cl:character . zlc:character)
	    (cl:close . zlc:close)
;	    (cl:decode-float . zlc:decode-float)
	    (cl:defstruct . zlc:defstruct)
	    (cl:delete . zlc:delete)
	    (cl:eval . zlc:eval)
	    (ticl:evalhook . zlc:evalhook)	; jlm 4/24/89
	    (cl:every . zlc:every)
	    (cl:float . zlc:float)
	    (cl:format . zlc:format)
	    (cl:intersection . zlc:intersection)
	    (cl:lambda . zlc:lambda)
	    (cl:listp . zlc:listp)
	    (cl:make-hash-table . zlc:make-hash-table)
	    (cl:map . zlc:map)
	    (cl:member . zlc:member)
	    (ticl:named-lambda . zlc:named-lambda)
	    (ticl:named-subst . zlc:named-subst)
	    (cl:nintersection . zlc:nintersection)
	    (ticl:nlistp . zlc:nlistp)
	    (cl:nunion . zlc:nunion)
	    (cl:package . zlc:package)	; jlm 4/24/89
	    (cl:rassoc . zlc:rassoc)
	    (cl:read . zlc:read)
	    (cl:read-from-string . zlc:read-from-string)
	    (cl:readtable . zlc:readtable)	; jlm 4/24/89
	    (cl:rem . zlc:rem)
	    (cl:remove . zlc:remove)
	    (cl:some . zlc:some)
            (cl:string . zlc:string)
;;;	    (cl:string= . zlc:string=)
;;;	    (cl:string-equal . zlc:string-equal)
	    (cl:subst . zlc:subst)
	    (cl:terpri . zlc:terpri)
	    (cl:union . zlc:union)
)
  "Alist used as *reader-symbol-substitutions* for reading Zetalisp code.")
