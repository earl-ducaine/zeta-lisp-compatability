;;; -*- Mode:Common-Lisp; Package:TRANSL; Base:10 -*-


(deftranslation when (form)
  (when (eq (car-safe (second form)) 'not) ; (WHEN (NOT x) y) ==> (UNLESS x y)
    (change form `(unless ,(second (second form)) . ,(rest2 form))) ) )

(deftranslation unless (form)
  (when (eq (car-safe (second form)) 'not) ; (UNLESS (NOT x) y) ==> (WHEN x y)
    (change form `(when ,(second (second form)) . ,(rest2 form))) ) )

(advise (:property if transform) :around improve-style nil
  (let (( form (first arglist) ))
    (if (= (length form) 3)		   ; (IF x y) ==> (WHEN x y)
	(progn (change (first form) 'when)
	       (translate form))
      :do-it)))

(advise (:property cond transform) :around improve-style nil
  (let (( form (first arglist) ))
    (cond ((null (rest2 form))		   ; (COND (x y)) ==> (WHEN x y)
	   (change form (cons 'when (second form)))
	   (translate form))
	  ((and (null (rest3 form))
		(null (rest2 (second form)))
		(eq (first (third form)) 't)
		(null (rest2 (third form)))) ; (COND (x y)(T z)) ==> (IF x y z)
	   (change form `(if ,(first (second form))
			     ,(second (second form))
			   ,(second (third form))) )
	   (translate form) )
	  (t :do-it) )))