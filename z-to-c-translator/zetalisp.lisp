


(defun validate-function-spec (function-spec &aux handler)
  "Predicate for use with check-arg.  returns non-nil if function-spec
   really is one. the value is the type of function spec (t for a
   symbol)."
  (cond ((atom function-spec)
	 (symbolp function-spec))
	((and (symbolp (car function-spec))
	      (setq handler (get (car function-spec) 'function-spec-handler))
	      (funcall handler 'validate-function-spec function-spec))
	 (car function-spec))))

(defun standardize-function-spec (function-spec &optional (errorp t))
  ;; Convert old maclisp-style property function specs
  (if (symbolp function-spec)
      (return-from standardize-function-spec function-spec)
      (and (consp function-spec)
	   (= (length function-spec) 2)
	   (symbolp (car function-spec))
	   (not (get (car function-spec) 'function-spec-handler))
	   (setq function-spec (cons :property function-spec))))
  (or (not errorp)
      (validate-function-spec function-spec)
      (error "~s is not a valid function spec." function-spec))
  function-spec)

(defun defun-compatibility (exp)
  "Process the cdr of a defun-form, converting old Maclisp formats
   into modern Lispm ones.  This must be done before the name of the
   function can be determined with certainty.  The value is an entire
   form, starting with defun or macro.  If no change has been made,
   the cdr of the value will be EQ to the argument."
  ;; 08/09/86 DNG - Use string-equal to compare for fexpr or expr
  ;; since these symbols won't be global.
  (prog (fctn-name ll body type tm)
     (setq type 'expr)
     (setq fctn-name (car exp))
     ;; convert list function specs
     (cond ((not (atom fctn-name))
	    ;; (defun (foo macro) ...)
	    (cond ((and (= (length fctn-name) 2)
			(eq (second fctn-name) 'macro))
		   (setq type 'macro fctn-name (car fctn-name)))
		  ((eq fctn-name (setq fctn-name
				       (standardize-function-spec fctn-name)))
		   ;; return if no conversion required
		   (return (cons 'defun exp)))))
	   ;; detect a valid defun.
	   ((or (not (atom (cadr exp))) (null (cadr exp)))
	    (return (cons 'defun exp)))
	   ((setq tm (member (cadr exp) '(fexpr expr macro) :test #'string-equal))
	    (setq type (car tm) exp (cdr exp)))
	   ((setq tm (member fctn-name '(fexpr expr macro) :test #'string-equal))
	    (setq type (car tm) fctn-name (cadr exp) exp (cdr exp))))
     ;; here if a new defun has to be constructed
     (setq ll (cadr exp))
     (setq body (cddr exp))
     ;; weird conversion hack to unconvert interlisp nlambdas that
     ;; were previously converted by holloway's random hacker to
     ;; kludgy fexpr's
     (cond ((and (eq type 'fexpr)
		 (equal ll '(*args*)))
	    (setq type 'expr)
	    ;; lambda list of internal lambda
	    (setq ll (cons '&quote (cadaar body)))
	    ;; body of internal lambda
	    (setq body (cddaar body)) ))
     ;; **end of that hack**
     (cond ((eq type 'fexpr)
	    (setq ll (cons '&quote (cons '&rest ll))))
	   ((eq type 'macro)
	    (return (cons 'macro (cons fctn-name (cons ll body)))))
	   ((and ll (atom ll))
	    (setq type 'lexpr
		  ll `(&eval &rest *lexpr-arglist* &aux
			     (,ll (length *lexpr-arglist*))))))
     (return (cons 'defun (cons fctn-name (cons ll body))))))
