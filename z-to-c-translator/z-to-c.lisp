;; -*- Mode:Common-Lisp; Package:(TRANSL :use (lisp ticl)); Base:10 -*-

;; here are the transforms for Zetalisp

;;  1/14/89 DNG - Fix handling of CATCH and THROW.

         ;; A

(defreplace	zl:add1               1+)

(deftranslation zl:aloc (form)
  (change form `(locf (aref ,@(cdr form)))))
(deftranslation zl:ap-leader (form)
  (change form `(locf (array-leader ,@(cdr form)))))
(deftranslation zl:ap-1 (form)
  (change form `(locf (ar-1 ,@(cdr form)))))
(deftranslation zl:ap-2 (form)
  (change form `(locf (ar-2 ,@(cdr form)))))
(deftranslation zl:ap-3 (form)
  (change form `(locf (ar-3 ,@(cdr form)))))

(defreplace     zl:ar-1               aref)
(defreplace     zl:ar-2               aref)
(defreplace     zl:ar-3               aref)
(defreplace     zl:array-#-dims       array-rank)
(deftranslation zl:array-grow (form &aux local)
  (change form (if (nthcdr 3 form)
		   `(with-stack-list ( ,(setf local (gentemp "LOCAL-")) ,@(nthcdr 2 form))
		      (adjust-array ,(second form)
				     ,local))
		   `(adjust-array ,(second form)
				   ,(third form)))))

(defreplace	zl:array-length       array-total-size)
(defreplace	zl:array-pop          vector-pop)
(deftranslation zl:array-push (form)
  (if (compiler:independent-expressions-p (second form) (third form))
      (change form `(vector-push ,(third form) ,(second form)))
      (change form `(warning " The arguments have been swapped during the translation,
make sure it can be done." (vector-push ,(third form) ,(second form))))))

(deftranslation zl:array-push-extend (form)
  (if (compiler:independent-expressions-p (second form) (third form))
      (change form `(vector-push-extend  ,(third form) ,(second form) ,@(nthcdr 3 form)))
      (change form `(warning " The arguments have been swapped during the translation,
make sure it can be done." (vector-push-extend   ,(third form) ,(second form),@(nthcdr 3 form) ))))
  )

(deftranslation zl:ar-2-reverse (form)
  (if (compiler:independent-expressions-p (fourth form) (third form))
      (change form `(aref ,(second form) ,(fourth form) ,(third form)))
      (change form `(warning " The arguments have been swapped during the translation,
make sure it can be done." (aref ,(second form) ,(fourth form) ,(third form))))))

(deftranslation zl:as-2-reverse (form)
  (if (compiler:independent-expressions-p (second form) (third form))
      (change form `(setf (ar-2-reverse  ,(third form) ,@(nthcdr 3 form)) ,(second form)))
      (change form `(warning " The arguments have been swapped during the translation,
make sure it can be done." (setf (ar-2-reverse ,(third form) ,@(nthcdr 3 form)) ,(second form))))))

(deftranslation zl:as-1 (form)
  (change (car form) 'zl:aset)
  (translate form))
(deftranslation zl:as-2 (form)
  (change (car form) 'zl:aset)
  (translate form))
(deftranslation zl:as-3 (form)
  (change (car form) 'zl:aset)
  (translate form))

(deftranslation zl:aset (form)
  (if (compiler:independent-expressions-p (second form) (third form))
      (change form `(setf (aref ,(third form) ,@(nthcdr 3 form)) ,(second form)))
      (change form `(warning " The arguments have been swapped during the translation,
make sure it can be done." (setf (aref ,(third form) ,@(nthcdr 3 form)) ,(second form))))))
(deftranslation zl:assq (form) (change form `(assoc ,@(cdr form) :test #'eq)))
(deftranslation zl:ass (form) (change form `(assoc ,(third form) ,(fourth form) :test ,(second form))))
(deftranslation zl:assoc (form) (change form `(assoc ,@(cdr form) :test #'equal)))
(defreplace zl:atan2 atan)
        ;; B

(defreplace	zl:bit-test           logtest)
	
	;; C
(deftranslation zl:caseq (form)
  (change (first form) 'case)
    (dothis (second form) form)
  (loop for i in (cddr form)
	do (dothese (cdr i) form))
  t) ;stop the sweep

(deftranslation zl:catch  (form )
  (when (AND (<= (LENGTH FORM) 3)
	     (CONSP (SECOND FORM))
	     (ATOM (THIRD FORM))
	     (OR (NOT (EQ #!C 'CATCH #!Z 'CATCH))
		 (AND (zetalisp-on-p)
		      (NEQ (FIRST (SECOND FORM)) 'QUOTE)
		      )))
    ;; Looks like old MacLisp style of CATCH.
    (change form `(catch ',(third form) ,(second form)))))

(defreplace	zl:*catch             catch)

(defreplace     zl:char             char>=)
(defreplace     zl:char             char<=)

(defreplace     zl:check-arg-type    check-type)
(defreplace     zl:clrhash-equal     clrhash)
;;; Code walker for condition-bind
(deftranslation zl:condition-bind (form)
  (dolist (handler (cadr-safe form))
    (dothis (cadr-safe handler) form))
  (dothese (cddr form) form))
;;; Code walker for condition-case
(deftranslation condition-case (form)
  (dothis (cadr-safe (cdr-safe form)) form) ;do the body-form
  (loop for i in (cdddr form)
	do (dothese (cdr i) form))
  t)
(defreplace	zl:copyalist          copy-alist)
(defreplace	zl:copylist           copy-list)
(defreplace	zl:copysymbol         copy-symbol)
(defreplace	zl:copytree           copy-tree)
	
	;; D
;	debug-io           *debug-io*
(deftranslation def (form) ;; to get aoround the bad macroexpander.
  (dothese (cddr form) form))
(defreplace	zl:defconst           defparameter)
(deftranslation zl:defstruct (form)
  (let
    ((conc-name '((:conc-name nil)))
     (callable-constructors '((:callable-constructors nil)))
     (alterant `((:alterant ,(intern (concatenate 'string
					  "ALTER-"
					  (string (if (consp (second form))
						      (car (second form))
						      (second form))))))))
     (predicate '((:predicate nil)))
     (copier '((:copier nil)))
     (type '((:type :array)))
     (name-and-options (second form)))
    (when (consp name-and-options)
      (dolist (option (cdr name-and-options))
	(case (if (consp option ) (car option ) option)
	      (:conc-name (if (consp option)
			      (setf conc-name nil)
			      (setf conc-name
				    `((:conc-name ,(intern (concatenate
							     'string
							     (string (car name-and-options)) "-")))))))
	      (:callable-constructors (setf callable-constructors nil))
	      (:alterant (setf alterant nil))
	      (:predicate (setf predicate nil))
	      (:copier (setf copier nil))
	      (:type (setf type nil))
	      (t (if (and (symbolp option)
			  (get (intern option 'keyword) 'si:defstruct-type-description))
		     (setf type `((:type ,option)))))))
      (delete (cadar type) (the list name-and-options) :test #'eq :count 1)
      (when conc-name 
      (delete :conc-name (the list name-and-options ) :test #'eq :count 1)))
    (dolist (slot (cddr form))
      (if (and (consp slot) (cdr slot))
	  (dothis (second slot) form)))

    (change form
	    `(defstruct ,(append (if (consp name-and-options)
				     name-and-options
				     (list  name-and-options))
				 conc-name
				 callable-constructors
				 alterant
				 predicate
				 copier
				 type )
	       ,@(nthcdr 2 form)))
    t))
    
(deftranslation zl:defunp (form)
  (change form (macroexpand form))
  (translate form)
  t)
(deftranslation zl:delq  (form )
  (change form `(delete ,(second form)
			(the list ,(third form))
			,.(and (fourth form) `(:count ,(fourth form)))
			:test #'eq)))
(deftranslation zl:delete  (form )
  (change form `(delete ,(second form)
			(the list ,(third form))
			,.(and (fourth form) `(:count ,(fourth form)))
			:test #'equal)))
(deftranslation zl:del  (form )
  (change form `(delete ,(third form)
			(the list ,(fourth form))
			,.(and (fifth form) `(:count ,(fifth form)))
			:test ,(second form))))
(defreplace	zl:del-if             delete-if)
(defreplace	zl:del-if-not         delete-if-not)

(deftranslation zl:deletef (form)
  (change form `(delete-file ,(second form)
			     ,@(if (>= (length form) 3) `(:error ,(third form)) nil)
			     ,@(if (>= (length form) 4) `(:query ,(fourth form)) nil))))

(deftranslation zl:deposit-byte (form)
  (change form `(dpb ,(fifth form) (byte ,(fourth form) ,(third form)) ,(second form))))

(deftranslation zl:difference (form)
  (if (= (length form) 2)
      (change (car form) 'identity )
      (change (car form) '-)))

(deftranslation zl:do (form)
  (if (and (atom (second form))
	   (second form))
      (progn
	(dothese (cdr form) form)
	(change form `(do ((,(second form ) ,(Third form) ,(fourth form)))
			  (,(fifth form) nil)
			,@(nthcdr 5 form)))
	;;phd 2/25 try to keep the comments in line
	(let ((comments (gethash form *hook-hash*)))
	  (when comments
	    (dolist (comment comments )
	      (setf (car comment )
		    (cond ((= 1 (car comment))
			   1)
			  (t
			   (max 2 (- (car comment) 2))))))))
	t)
      (progn               ;; Zetalisp do
	(loop for i in  (second form)
       do (dothese i form))
	(dothese (third form) form)
	(dothese (nthcdr 3 form)form)
	t)))
(deftranslation zl:do-named (form)
  (change form `(block ,(cadr-safe form)
		  (do ,@(nthcdr 2 form)))))
(deftranslation zl:do-named* (form)
  (change form `(block ,(cadr-safe form)
		  (do* ,@(nthcdr 2 form)))))
(deftranslation zl:do*-named (form)
  (change form `(block ,(cadr-safe form)
		  (do* ,@(nthcdr 2 form)))))

(defreplace	zl:do-forever         loop)
(deftranslation zl:do-all-symbols (form)
  (dothese (cddr form ) form)
  (when (consp (second form))
    (when (>= (length (second form)) 2)
      (dothis (second (second form)) form))
    )
  t)
(deftranslation zl:do-symbols (form)
  (dothese (cddr form ) form)
  (when (consp (second form))
    (when (>= (length (second form)) 2)
      (dothis (second (second form)) form))
    (when (>= (length (second form)) 3)
      (dothis (third (second form)) form)))
  t) ;stop the sweep

(deftranslation zl:do-external-symbols (form)
  (dothese (cddr form ) form)
  (when (consp (second form))
    (when (>= (length (second form)) 2)
      (dothis (second (second form)) form))
    (when (>= (length (second form)) 3)
      (dothis (third (second form)) form)))
  t)
	
	;; E
;	error-output       *error-output*

(deftranslation zl:eval (form)
  (dothese (cdr form) form)
  (change form `(warning "Make sure that the argument to eval is a common lisp form"
			 (eval ,@(cdr form))))
  t)

(deftranslation eval-when (form)
  (dothese (cddr form) form))

(deftranslation zl:every (form)
    (if (fourth form)
	(let ((tmp (gentemp "AUX-VAR")))
	  (change form `(loop for ,tmp in ,(second form) by ,(fourth form) always (funcall ,(third form) ,tmp))))
      (if (compiler:independent-expressions-p (third form)(second form))
	  (change form `(every ,(third form) ,(second form)))
	  (progn (dothese (cdr form) form)
		 (change form `(warning " The arguments have been swapped during the translation,
make sure it can be done." (every ,(third form) ,(second form))))
		 t))))

(deftranslation zl:error (form)
  (if (stringp (second  form))
      (change form `(error  ,(second form)))
      (change form  `(ferror ,@(cdr  form)))))
	
	;; F
; (defreplace     ferror             error) ;first approximation
(deftranslation zl:find-position-in-list (form)
  (change form `(position ,(second form) (the list ,(third form))
			  :test #'eq)))

(deftranslation zl:find-position-in-list-equal (form)
  (change form `(position ,(second form) (the list ,(third form))
			  :test #'equal)))
(deftranslation zl:fix (form)
  (change form `(values (floor ,@(rest form)))))

(defreplace	zl:fixp               integerp)
(deftranslation zl:fixr (form)
  (change form `(values (round ,@(rest form)))))
(deftranslation zl:fset-carefully (form)
  (change form `(fdefine ,(second form) ,(third form) t ,@(nthcdr 3 form))))

(defreplace	zl:fsymeval           symbol-function)

(deftranslation zl:funcall-self (form)
  (change form `(send self ,@(cdr form))))

(deftranslation zl:lexpr-funcall-self (form)
  (change form `(apply self ,@(cdr form))))

	;; G

(deftranslation zl:get (form)
  (if (and (consp (second form))
	   (eq (car (second form)) 'locf))
      (change form `(getf ,(second (second form)) ,@(nthcdr 2 form)))))
(defreplace zl:gethash-equal          gethash)
(defreplace zl:get-from-alternating-list getf)
(defreplace	zl:get-pname          symbol-name)
(defreplace	zl:greaterp           >)
        ;;I
(defreplace zl:intern-soft find-symbol)
(deftranslation zl:if (form)
  (when (> (length form) 4)
    ;;phd 2/25/86 hack up the comments so they come out right.
    (let((new-list `(progn ,@(nthcdr 3 form))))
      (multiple-value-bind (comments nil pointer)
	  (gethash form *hook-hash*)
	(when comments
	  (do ((comm comments (cdr comm))
	       (last nil))
	      ((null comm))
	    (if  (<= 4 (caar comm))
		 (progn 
		   (setf (caar comm) (-  (caar comm ) 2))
		   (if last    
		       (setf (cdr last ) (cdr comm))
		       (setf (second pointer) (cdr comm)))
		   (push (car comm) (gethash new-list *hook-hash*)))
		 (setf last comm )))))
      (change form `(if ,(second form) ,(third form) ,new-list)))))

(deftranslation zl:intersection (form)
  (if (> (length form)
	 3)
      (progn
	(dothese (cdr form ) form)
	(change form (reduce #'(lambda (a b) `(intersection ,a ,b :test #'eq))
			      (cdr form)))
	t) ; return t to stop sweep
      (change form `(intersection ,@(cdr form) :test #'eq))))

     
         ;; K
(defreplace zl:pkg-kill kill-package)
	
	;; L
(defreplace	zl:lessp              <)
(defreplace	zl:lexpr-funcall      apply)
(deftranslation zl:load-byte (form)
  (change form `(ldb (byte ,(fourth form) ,(third form)) ,(second form))))
(defreplace	zl:listp              consp)
(deftranslation zl:local-declare (form)
  (change form `(locally (declare ,@(cadr-safe form))
			 ,@(cddr form))))

(deftranslation zl:loop (form)
  ;; Do that to get around the screwy macroexpansion of loop.
  (dothese (cdr form) form))
	;; M

(deftranslation zl:make-array (form)
  (do ((items (nthcdr 2 form ) (cddr items))
       tmp)
      ((null items))
    (when (and (eq :type (car items))
	       (or (eq (car-safe (cadr items ) ) 'quote) (symbolp (cadr items ) ))
	       (setf tmp (RASSoc
			   (if (symbolp (cadr items )) (cadr items)
			       (cadr-safe (cadr items)))
			   '((BIT . ART-1B)
			     (T . ART-Q)
			     (STRING-CHAR . ART-STRING)
			     ((SIGNED-BYTE 16) . ART-HALF-FIX)
			     (FLOAT . ART-FLOAT)
			     (FAT-CHAR . ART-FAT-STRING)
			     ((COMPLEX FLOAT) . ART-COMPLEX-FLOAT)
			     (COMPLEX . ART-COMPLEX)
			     (FIXNUM . ART-FIX)
			     ((UNSIGNED-BYTE 1) . ART-1B)
			     ((UNSIGNED-BYTE 2) . ART-2B)
			     ((UNSIGNED-BYTE 4) . ART-4B)
			     ((UNSIGNED-BYTE 8) . ART-8B)
			     ((UNSIGNED-BYTE 16) . ART-16B)
			     ((UNSIGNED-BYTE 32) . ART-32B)
			     ((SIGNED-BYTE 16) . ART-HALF-FIX))
					  :test #'eq )))
      (change (car items ) :element-type)
      (change (cadr items) `',(car tmp))
      (return nil))))

(deftranslation zl:make-equal-hash-table (form)
  (change form `(make-hash-table :test #'equal ,@(rest form))))
(deftranslation make-hash-table (form)
  (let ((hash-function '(:hash-function nil))
	(compare-function '(:compare-function #'eq)))
    (do ((keys (cdr form) (cddr keys)))
	((null keys))
      (case (car keys)
	    (:test (setf hash-function nil
			 compare-function nil))
	    (:hash-function (setf hash-function nil))
	    (:compare-function (setf compare-function  nil))
	    (otherwise nil)))
  (change form `(make-hash-table ,@compare-function ,@hash-function
				 ,@(cdr form)))))

(defreplace     zl:make-syn-stream    make-synonym-stream)
(defreplace	zl:map                mapl)
(defreplace     zl:maphash-equal      maphash)
(deftranslation zl:member (form ) (change form `(member ,@(cdr form) :test #'equal)))
(deftranslation zl:memq (form ) (change form `(member ,@(cdr form) :test #'eq)))
(deftranslation zl:mem (form) (change form `(member ,(third form) ,(fourth form) :test ,(second form))))
(defreplace	zl:minus              -)

(deftranslation	zl:multiple-value     (form )
  (change (first form) 'multiple-value-setq)
  (dothis (third form ) form))
	
(deftranslation multiple-value-bind (form)
  (dothese (nthcdr 2 form) form))

(deftranslation multiple-value-setq (form)
  (dothis (third form) form))
(deftranslation multiple-value-list (form)
  (dothis (cadr-safe form) form))
(deftranslation multiple-value-call (form)
  (dothese (rest form) form))
	;; N
(deftranslation zl:ncons (form)
  (change form `(cons ,(second form) nil)))
(deftranslation zl:ncons-in-area (form)
  (change form `(cons-in-area ,(second form) nil ,(third form))))
(deftranslation zl:nintersection (form)
  (if (> (length form)
	 3)
      (progn
	(dothese (cdr form ) form)
	(change form (reduce #'(lambda (a b) `(nintersection ,a ,b :test #'eq))
			      (cdr form)))
	t) ; return t to stop sweep
      (change form `(reduce ,@(cdr form) :test #'eq))))

(defreplace zl:nlistp atom)

(deftranslation zl:nunion (form)
  (if (> (length form)
	 3)
      (progn
	(dothese (cdr form ) form)
	(change form (reduce #'(lambda (a b) `(nunion ,a ,b :test #'eq))
			      (cdr form)))
	t) ; return t to stop sweep
      (change form `(nunion ,@(cdr form) :test #'eq))))

(deftranslation si:nunion-equal (form)
  (if (> (length form)
	 3)
      (progn
	(dothese (cdr form ) form)
	(change form (reduce #'(lambda (a b) `(nunion ,a ,b :test #'equal))
			      (cdr form)))
	t) ; return t to stop sweep
      (change form `(nunion ,@(cdr form) :test #'equal))))

	
	;; P
(defreplace	zl:plist              symbol-plist)
(defreplace	zl:plus               +)
(defreplace     zl:pkg-name           package-name)
(defreplace	zl:probef             probe-file)

(deftranslation  zl:puthash (form)
  (change form `(setf (gethash ,(second form) ,(fourth form)) ,(third form) ,@(nthcdr 4 form))))

(deftranslation  zl:puthash-equal (form)
  (change form `(setf (gethash ,(second form) ,(fourth form)) ,(third form) ,@(nthcdr 4 form))))

(deftranslation zl:put-on-alternating-list (form)
  (if (and (compiler:independent-expressions-p (second form) (fourth form))
	   (compiler:independent-expressions-p (second form) (third form)))
      (change form `(setf (getf ,(third form) ,(fourth form)) ,(second form)))
      (change form `(warning " The arguments have been swapped during the translation,
make sure it can be done." (setf (getf ,(third form) ,(fourth form)) ,(second form))))))

(deftranslation zl:putprop (form)
  (if (compiler:independent-expressions-p (third form) (fourth form))
      (change form `(setf (get ,(second form) ,(fourth form)) ,(third form)))
      (change form `(warning " The arguments have been swapped during the translation,
make sure it can be done." (setf (get ,(second form) ,(fourth form)) ,(third form))))))
	;; Q
;	query-io           *query-io*
(deftranslation zl:quotient (form)
  (change (car form ) 'global:/)
  (translate form) t)

	;; R
(deftranslation zl:rass (form)
  (change form `(rassoc ,(third form) ,(fourth form) :test  ,(second form))))
(deftranslation zl:rassoc (form )
  (change form `(rassoc ,@(cdr form) :test #'equal)))
(deftranslation zl:rassq (form)
  (change form `(rassoc ,(second form) ,(third form)  :test #'eq)))
(deftranslation zl:read (form)
  (if (>= 2   (length form))
	 (change form `(read ,(second form) t))
	 (change form `(read ,(second form) nil ,(third form)))))

(deftranslation zl:read-from-string (form)
  (change form `(read-from-string ,(second form) ,.(if (< 2 (length  form)) `(nil ,(third form))  `(t))
				  ,.(if (fourth form) `(:start ,(fourth form)) nil)
				  ,.(if (fifth form) `(:stop ,(fifth form)) nil))))
(deftranslation zl:remq  (form )
  (change form `(remove ,(second form)
			(the list ,(third form))
			,.(and (fourth form) `(:count ,(fourth form)))
			:test #'eq)))
(deftranslation zl:remove  (form )
  (change form `(remove ,(second form)
			(the list ,(third form))
			,.(and (fourth form) `(:count ,(fourth form)))
			:test #'equal)))
(deftranslation zl:rem  (form )
  (change form `(remove ,(third form)
			(the list ,(fourth form))
			,.(and (fifth form) `(:count ,(fifth form)))
			:test ,(second form))))
(defreplace zl:\\ remainder)
(defreplace     zl:remhash-equal      remhash)
(defreplace	zl:rem-if             remove-if)
(defreplace	zl:rem-if-not         remove-if-not)
(defreplace	zl:renamef            rename-file)
(deftranslation zl:return (form)
  (when (/= (length form) 2)
    (change form `(return (values ,@(rest form))))))
(deftranslation zl:return-list (form)
  (change form `(return (values-list ,(second form)))))

(deftranslation zl:return-array (form)
  (unless (eq (car-safe (cadr form)) 'prog1)
    (change form `(return-array (prog1 ,(cadr-safe form) (setf ,(cadr-safe form) nil))))))
(deftranslation zl:return-storage (form)
  (unless (eq (car-safe (cadr form)) 'prog1)
    (change form `(return-storage (prog1 ,(cadr-safe form) (setf ,(cadr-safe form) nil))))))

(deftranslation zl:rest1 (form)
  (change form `(nthcdr 1 ,(second form))))
(deftranslation zl:rest2 (form)
  (change form `(nthcdr 2 ,(second form))))
(deftranslation zl:rest3 (form)
  (change form `(nthcdr 3 ,(second form))))
(deftranslation zl:rest4 (form)
  (change form `(nthcdr 4 ,(second form))))
	
	;; S

(defreplace zl:samepnamep     string=)
(deftranslation zl:selectq
 (form)
  (change (first form) 'case)
    (dothis (second form) form)
  (loop for i in (cddr form)
	do (dothese (cdr i) form))
  t)
;	standard-input     *standard-input*
;	standard-output    *standard-output*

(deftranslation send (form) ;; to override the macroexpansion of send.
  (declare (ignore form))
  nil) 

(deftranslation zl:setplist (form)
  (change form `(setf (symbol-plist ,(second form)) ,(third form))))

(deftranslation zl:special (form)
  (change form `(proclaim '(special ,@(cdr form)))))

(deftranslation zl:some (form)
    (if (fourth form)
	(let ((tmp (gentemp "AUX-VAR")))
	  (change form `(loop for ,tmp on ,(second form) by ,(fourth form) when (funcall ,(third form) (car ,tmp))
			      return ,tmp)))
	(if (compiler:independent-expressions-p (third form)(second form))
	  (change form `(some ,(third form) ,(second form)))
	  (progn (dothese (cdr form) form)
		 (change form `(warning " The arguments have been swapped during the translation,
make sure it can be done." (some ,(third form) ,(second form))))
		 t))))

(deftranslation zl:sortcar (form)
  (change form `(sort ,@(rest form) :key #'car)))

(defreplace     zl:string             string>=)
(defreplace     zl:string             string<=)
(defreplace     zl:string             string/=)

(deftranslation zl:string-equal (form )
  (unless (some #'keywordp form)
    (change form `(string-equal ,(second form) ,(third form)
				,@(do ((args (cdddr form) (cdr args))
				       (l nil)
				       (keys '(:start1 :start2 :end1 :end2) (cdr keys)))
				      ((null args) l)
				    (push (car args) l)
				    (push (car keys) l))))))
(deftranslation zl:string= (form )
  (unless (some #'keywordp form)
    (change form `(string= ,(second form) ,(third form)
			   ,@(do ((args (cdddr form) (cdr args))
				  (l nil)
				  (keys '(:start1 :start2 :end1 :end2) (cdr keys)))
				 ((null args) l)
			       (push (car args) l)
			       (push (car keys) l))))))
				       
(defreplace zl:string-length length)
(deftranslation zl:string-reverse (form)
  (change form `(reverse  (the string ,(second form)))))
(deftranslation zl:string-nreverse (form)
  (change form `(nreverse (the string ,(second form)))))

(deftranslation zl:string-search-char (form)
  (change form `(position ,(second form) (string ,(third form))
		      ,@(if (>= (length form) 4)
			    `(:start ,(fourth form)))
		      ,@(if (>= (length form) 5)
			    `(:end ,(fifth form)))
		      ,@(if (>= (length form) 6)
			    (if (symbolp (sixth form))
				(cond((eq (sixth form) t)
				      `(:test #'char=))
				     ((eq (sixth form) nil)
				      `(:test #'char-equal))
				     (t `(:test (if ,(sixth form) #'char= #'char-equal))))
				`(:test (if ,(sixth form) #'char= #'char-equal)))
			    `(:test #'char-equal)))))

(deftranslation zl:string-search-not-char (form)
  (change form `(position ,(second form) (string ,(third form))
		      ,@(if (>= (length form) 4)
			    `(:start ,(fourth form)))
		      ,@(if (>= (length form) 5)
			    `(:end ,(fifth form)))
		      ,@(if (>= (length form) 6)
			    (if (symbolp (sixth form))
				(cond((eq (sixth form) t)
				      `(:test-not #'char=))
				     ((eq (sixth form) nil)
				      `(:test-not #'char-equal))
				     (t `(:test-not (if ,(sixth form) #'char= #'char-equal))))
				`(:test-not (if ,(sixth form) #'char= #'char-equal)))
			    `(:test-not #'char-equal)))))

(deftranslation zl:string-search (form)
  (change form `(search (string ,(second form)) (string ,(third form))
		      ,@(if (>= (length form) 4)
			    `(:start2 ,(fourth form)))
		      ,@(if (>= (length form) 5)
			    `(:end2 ,(fifth form)))
		      ,@(if (>= (length form) 6)
			    `(:start1 ,(sixth form)))
		      ,@(if (>= (length form) 7)
			    `(:end1 ,(seventh form)))
		      ,@(if (>= (length form) 8)
			    (if (symbolp (eighth form))
				(cond((eq (eighth  form) t)
				      `(:test #'char=))
				     ((eq (eighth  form) nil)
				      `(:test #'char-equal))
				     (t `(:test (if ,(eighth  form) #'char= #'char-equal))))
				`(:test (if ,(eighth  form) #'char= #'char-equal)))
			    `(:test #'char-equal)))))

(deftranslation zl:string-reverse-search (form)
  (change form `(search (string ,(second form)) (string ,(third form))
			:from-end t
		      ,@(if (>= (length form) 4)
			    `(:end2 ,(fourth form)))
		      ,@(if (>= (length form) 5)
			    `(:start2 ,(fifth form)))
		      ,@(if (>= (length form) 6)
			    `(:start1 ,(sixth form)))
		      ,@(if (>= (length form) 7)
			    `(:end1 ,(seventh form)))
		      ,@(if (>= (length form) 8)
			    (if (symbolp (eighth form))
				(cond((eq (eighth  form) t)
				      `(:test #'char=))
				     ((eq (eighth  form) nil)
				      `(:test #'char-equal))
				     (t `(:test (if ,(eighth  form) #'char= #'char-equal))))
				`(:test (if ,(eighth  form) #'char= #'char-equal)))
			    `(:test #'char-equal)))))

(deftranslation zl:string-reverse-search-char (form)
  (change form `(position ,(second form) (string ,(third form))
			  :from-end t
		      ,@(if (>= (length form) 4)
			    `(:end,(fourth form)))
		      ,@(if (>= (length form) 5)
			    `(:start ,(fifth form)))
		      ,@(if (>= (length form) 6)
			    (if (symbolp (sixth form))
				(cond((eq (sixth form) t)
				      `(:test #'char=))
				     ((eq (sixth form) nil)
				      `(:test #'char-equal))
				     (t `(:test (if ,(sixth form) #'char= #'char-equal))))
				`(:test (if ,(sixth form) #'char= #'char-equal)))
			    `(:test #'char-equal)))))

(deftranslation zl:string-reverse-search-not-char (form)
  (change form `(position ,(second form) (string ,(third form))
			  :from-end t
		      ,@(if (>= (length form) 4)
			    `(:end ,(fourth form)))
		      ,@(if (>= (length form) 5)
			    `(:start ,(fifth form)))
		      ,@(if (>= (length form) 6)
			    (if (symbolp (sixth form))
				(cond((eq (sixth form) t)
				      `(:test-not #'char=))
				     ((eq (sixth form) nil)
				      `(:test-not #'char-equal))
				     (t `(:test-not (if ,(sixth form) #'char= #'char-equal))))
				`(:test-not (if ,(sixth form) #'char= #'char-equal)))
			    `(:test-not #'char-equal)))))


(defreplace	zl:sub1               1-)

(deftranslation zl:subset (form)
  (when (= (length form) 3)
      (change (first form) 'remove-if-not)))

(deftranslation zl:subset-not (form)
  (when(= (length form) 3)
      (change (first form) 'remove-if)))

(deftranslation zl:substring (form)
  (if (<= (length form ) 4) ;area arg omitted
    (change form `(subseq (string ,(second form)) ,@(nthcdr 2 form )))
    (change form `(let ((default-cons-area ,(fifth form)))
		    (subseq (string ,(second form)) ,(third form )
			    ,(fourth form))))))

(deftranslation zl:subrp (form)
  (change form `(typep ,(second form) '(or compiled-function microcode-function))))
(deftranslation zl:subst (form)
  (dothese (cdr form) form)
  (change form `(warning "Copy-tree has been added for safety, see if you really need it"
			 (subst ,(second form) ,(third form) (copy-tree ,(fourth form)) :test #'equal)))
  t)
(defreplace     zl:swapf              rotatef)
(defreplace     zl:swaphash-equal     swaphash)
(defreplace	zl:symeval            symbol-value)
	
	;; T
;	terminal-io        *terminal-io*
(deftranslation zl:throw  (form )
  (when (AND (<= (LENGTH FORM) 3)
	     (CONSP (SECOND FORM))
	     (ATOM (THIRD FORM))
	     (OR (NOT (EQ #!C 'THROW #!Z 'THROW))
		 (AND (zetalisp-on-p)
		      RUN-IN-MACLISP-SWITCH)))
    (change form `(throw ',(third form) ,(second form)))))
(defreplace	zl:*throw             throw)
(defreplace	zl:times              *)
;	trace-output       *trace-output*
(deftranslation zl:tyo (form)
  (if (characterp (second form))
      (change form `(write-char ,@(nthcdr 1 form)))
      (change form `(write-char (int-char ,(second form)) ,@(cddr form)))))
(deftranslation zl:typep (form)
  (when (= (length form) 2)
    (change (car form) 'type-of)))

        ;; U
(deftranslation zl:union (form)
  (if (> (length form)
	 3)
      (progn
	(dothese (cdr form ) form)
	(change form (reduce #'(lambda (a b) `(union ,a ,b :test #'eq))
			      (cdr form)))
	t) ; return t to stop sweep
      (change form `(union ,@(cdr form) :test #'eq))))

        ;; V
(defreplace zl:viewf           view-file)
         ;;X

(deftranslation zl:xcons-in-area (form)
  (change form `(let ((default-cons-area ,(third form)))
		  (cons ,(second form) ,(first form)))))

(deftranslation zl:xcons (form)
  (change form `(cons ,(second form) ,(first form)))) 

(deftranslation si:xr-bq-append (form)
  (when *translate-backquote* 
    (change form `(si:grind-bq ,(si:pp-unbackquotify form)))))

(deftranslation si:xr-bq-cons (form)
    (when *translate-backquote* 
      (change form `(si:grind-bq ,(si:pp-unbackquotify form)))))

(deftranslation si:xr-bq-list (form)
    (when *translate-backquote* 
      (change form `(si:grind-bq ,(si:pp-unbackquotify form)))
      ))

(deftranslation si:xr-bq-nconc (form)
  (when *translate-backquote*
    (change form `(si:grind-bq ,(si:pp-unbackquotify form)))))

(deftranslation si:xr-bq-list* (form)
    (when *translate-backquote* 
      (change form `(si:grind-bq ,(si:pp-unbackquotify form)))))

(deftranslation si:xr-bq-vector (form)
  (when *translate-backquote*
    (change form `(si:grind-bq ,(si:pp-unbackquotify form)))))

	;; Signs & Symbols
(defreplace	zl:+$                 +)
(defreplace	zl:-$                 -)
(defreplace	zl:*$                 *)
(defreplace     zl:/$                 /)
(defreplace	zl:^$                 EXPT)
(defreplace	zl:*dif               -)
(defreplace	zl:*plus              +)
(defreplace	zl:*times             *)
(defreplace	zl:1+$                1+)
(defreplace	zl:1-$                1-)
(defreplace	zl:^                  EXPT)
(defreplace	zl:                  >=)
(defreplace	zl:                  <=)
(defreplace     zl:\\\\               gcd)
(defreplace     zl:                  /=)
(deftranslation zl:/ (form)
  (unless (or (floatp (second form))
	    (floatp (if (>= (length form) 3) (third form))))
    (dothese (cdr form) form)
    (change  form `(warning "May be you can use / or ceiling or floor or truncate if you know 
The type of your arguments" (quotient ,@(cdr form))))
    t))
(defreplace     zl:%div               /)
;; Bytespect to (byte s p) conversion
(deftranslation ldb (form)
  (when (integerp (second form))
    (change (second form) `(byte ,(byte-size (second form)) ,(byte-position (second form))))))
(deftranslation %p-ldb-offset  (form)
  (when (integerp (second form))
    (change (second form) `(byte ,(byte-size (second form)) ,(byte-position (second form))))))
(deftranslation dpb (form)
  (when (integerp (third form))
    (change (third form) `(byte ,(byte-size (third form)) ,(byte-position (third form))))))
(deftranslation ldb-test (form)
  (when (integerp (second form))
    (change (second form) `(byte ,(byte-size (second form)) ,(byte-position (second form))))))
(deftranslation mask-field (form)
  (when (integerp (second form))
    (change (second form) `(byte ,(byte-size (second form)) ,(byte-position (second form))))))
(deftranslation deposit-field (form)
  (when (integerp (third form))
    (change (third form) `(byte ,(byte-size (third form)) ,(byte-position (third form))))))
