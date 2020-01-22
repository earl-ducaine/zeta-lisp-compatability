
(in-package :primitive-zlisp)

;; noop
(defun beep ())

(defun fquery (options fquery-format-string &rest fquery-format-args)
  "Ask a multiple-choice question on QUERY-IO.
   fquery-format-string and fquery-format-args are used to print the
   question. Ending the string with \"? \" is often appropriate.
   options is a plist. Defined indicators are:
   :make-complete boolean. Send a :make-complete message to the stream
     if it understands it.
   :type one of :tyi, :readline, :mini-buffer-or-readline.  It says
     how the answer is gathered and echoed.
   :choices a list of choices. A choice is either the symbol :any or
     a list.  If a list, its car is either a possible return value, or
     a list of a possible return value and how to echo it.  The
     remaining things in the list are input items that select that
     return value.  For a :readline type call, they should be strings.
     For a :tyi type call, they should be characters.  Example
     choice (for :readline): ((:foo \"Foo\") #F #\\space)
   :fresh-line boolean.  Send a :fresh-line to the stream initially.
   :condition symbol.  Signalled before asking.
   :list-choices boolean.  If T, a list of choices is printed after
     the question.
   :beep boolean. If t, we beep before printing the message.
   :clear-input boolean. If t, we discard type-ahead before printing the message.
   :select boolean.  Select the window and select back.
   :help-function specifies a function to be called if the user types
     help. It is called with stream, choices and type-function as arguments.
   :stream stream or expression. Specifies the stream to use.  If it
     is a symbol (which is not an io-stream) or a list it is
     evaluated.  Default is to use query-io."
  (let (make-complete type type-function fquery-choices stream
	fquery-stream fresh-line condition fquery-list-choices
	fquery-help-function beep-p clear-input select handled-p val
	typein)
    (setf
     (values make-complete type fquery-choices stream beep-p
	     clear-input select fresh-line condition
	     fquery-list-choices fquery-help-function)
     (apply 'fquery-decode-options options))
    (setq fquery-stream
	  (if stream
	      (if (or (and (symbolp stream)
			   (not (get stream 'si:io-stream-p)))
		      (consp stream))
		  (eval stream)
		  stream)
	      *query-io*))
    (setq type-function
	  (or (get type 'fquery-function)
	      (ferror () "~S is not a valid :TYPE for FQUERY" type)))
    (and condition
	 (or (neq condition 'fquery)
	     (eh:condition-name-handled-p condition))
	 (multiple-value-setq (handled-p val)
	   (signal-condition
	    (apply 'make-condition condition options
		   fquery-format-string fquery-format-args)
	    '(:new-value))))
    (if handled-p
	val
	(progn
	  (block top
	    (do ()
		(nil)
	      (when beep-p
		(beep))
	      (when clear-input
		(clear-input fquery-stream))
	      (when fresh-line
		(fresh-line fquery-stream))
	      (setq typein (funcall type-function :read fquery-stream))
	      (dolist (choice fquery-choices)
		(cond
		  ((eq choice :any)
		   (funcall type-function :echo typein fquery-stream)
		   ;; (when make-complete
		   ;;   (funcall fquery-stream :send-if-handles :make-complete))
		   (return-from top typein))
		  ((funcall type-function :member typein (cdr choice))
		   (setq choice (car choice))
		   (when (consp choice)
		     (funcall type-function :echo (cadr choice) fquery-stream)
		     (setq choice (car choice)))
		   (when make-complete
		     (funcall fquery-stream :send-if-handles :make-complete))
		   (return-from top choice))))
	      (setq beep-p t
		    clear-input t
		    ;; User spazzed, will need fresh line
		    fresh-line t
		    ;; and should list options
		    fquery-list-choices t)))))))

;; lm ferror could also write to some other output, other than
;; *error-output*, through paramter configuration. But it was seldom
;; used.
(defmacro ferror (file datum &rest arguments)
  (declare (ignore file))
  `(error ,datum ,@arguments))

(defun draw-char-cache-state (&optional draw-char-cache)
  (declare (ignore draw-char-cache))
  (error "Not implemented"))
;; (let ((draw-char-cache (or draw-char-cache *draw-char-cache*)))
;;   (%draw-char-cache-state draw-char-cache)))

;; Helper function for creating zetalisp-like structure accessors with
;; a default pointer.
(defmacro emulate-default-pointer (structure-name)
  (declare (ignore structure-name))
  (error "Not implemented"))

;; `(defun draw-char-cache-state (&optional draw-char-cache)
;;    (let ((draw-char-cache (or draw-char-cache *draw-char-cache*)))
;;      (%draw-char-cache-state draw-char-cache))))


(defun make-collector (&optional initial-value)
  "Create a collector function. A Collector function will collect,
   into a list, all the values passed to it in the order in which they
   were passed. If the callector function is called without arguments
   it returns the current list of values."
  (let ((value initial-value)
        (cdr (last initial-value)))
    (lambda (&rest items)
      (if items
          (progn
            (if value
                (if cdr
                    (setf (cdr cdr) items
                          cdr (last items))
                    (setf cdr (last items)))
                (setf value items
                      cdr (last items)))
            items)
          value))))

(defun partition (list &rest lambdas)
  "Split LIST into sub lists according to lambdas. Each element of
   list will be passed to each element of lambdas, the first function
   in lambdas which returns t will cause that element to be collected
   into the corresponding list.
   Examples:
   (partition '(1 2 3) #'oddp #'evenp) => ((1 3) (2))
   (partition '(1 2 3) #'oddp t) => ((1 3) (1 2 3))
   (partition '(1 2 3) #'oddp #'stringp) => ((1 3) nil)"
  (let ((collectors (mapcar (lambda (predicate)
                              (cons (case predicate
                                      ((t :otherwise)
                                       (constantly t))
                                      ((nil)
                                       (constantly nil))
                                      (t predicate))
                                    (make-collector)))
                            lambdas)))
    (dolist (item list)
      (dolist (test-func-collector-func collectors)
	(let ((test-func (car test-func-collector-func))
	      (collector-func (cdr test-func-collector-func)))
	  (when (funcall test-func item)
	    (funcall collector-func item)))))
    (mapcar #'funcall (mapcar #'cdr collectors))))

(deftype art-32b () '(unsigned-byte 32))

(defun memq (x y)
  (loop
     (cond ((atom y) (return nil))
	   ((eq x (car y)) (return y)))
     (pop y)))

(defun parse-defstruct-name-and-options (name-and-options)
  (let* ((name-and-options (if (atom name-and-options)
			       (list name-and-options)
			       name-and-options))
	 (defstruct-name (first name-and-options))
	 (defstruct-options (rest name-and-options))
	 ;; Extract :dfault-pointer option
	 (default-pointer (getf defstruct-options :default-pointer)))
    (remf defstruct-options  :default-pointer)
    (values `(list ,defstruct-name ,@defstruct-options) default-pointer)))

(defun extract-and-remove-default-pointer (defstruct-options)
  ;; Local functions used for partitioning options the following
  ;; overlapping categories:
  ;; 1) Options whose value we need, but are not valid/portable
  ;;    defstruct options in modern common lisp.
  ;; 2) Options whose value we need but are portable CL.
  ;; 3)
  (labels ()
    (nlet ((extract-and-remove-options
	    '(:alterant :default-pointer))
	   ;; Check to see if the option was set and its value, if
	   ;; applicable, but don't remove.
	   (query-options
	    '(:conc-name))
	   (partitioned-defstruct-options
	    (partition defstruct-options #'atom #'consp))
	   ((alist (cadr partitioned-defstruct-options))
	    ((atoms (car partitioned-defstruct-options))
	     (default-pointer (assoc :default-pointer alist))
	     (filtered-alist
	      (remove-if (lambda (item)
			   (and (consp item)
				(eq (car item) :default-pointer)))
			 alist)))))
      (declare (ignore extract-and-remove-options query-options))
      (format t "~s~%" default-pointer)
      ;; Reconstitute and return defstruct-options without
      ;; :default-pointer. Return :deault-pointer as a second value, nil
      ;; if not found.
      (values (concatenate 'list filtered-alist atoms)
    	      default-pointer))))

(defun generate-defstruct (name options slot-descriptions)
  `(cl:defstruct (,name ,@options) ,@slot-descriptions))

(defun generate-default-pointer-accessors (slot-descriptions)
  (declare (ignore slot-descriptions))
)

(defmacro defstruct (name-and-options &body slot-descriptions)
  (nlet ((name-and-options (if (atom name-and-options)
			       (list name-and-options)
			       name-and-options))
	 ((defstruct-name (first name-and-options))
	  (defstruct-options default-pointer alterant
	    (extract-and-remove-default-pointer (rest name-and-options)))))
    (declare (ignore default-pointer alterant))
	`(progn
	   ,(generate-defstruct defstruct-name defstruct-options
				slot-descriptions)
	   ,(generate-default-pointer-accessors slot-descriptions))))

(defvar *defstructs*)

(defun run-defstruct ()
  (dolist (defstruct *defstructs*)
    (format t "~s~%" (macroexpand defstruct))))

(defvar *condition-handlers* nil
  "List of active condition handlers. Each element is (condition-names
   function). condition-names is either a condition typespec,
   condition name, a list of names, or NIL for all conditions.")

(defvar *condition-default-handlers* nil
  "List of active default condition handlers. Each element
   is (condition-names function). condition-names is either a
   condition name, a list of names, or nil for all conditions. The
   handlers on this list are tried after all of
   *condition-handlers*.")

(defvar *condition-resume-handlers* nil
  "List of active resume handlers and active Common LISP restarts.
   For example, (<resume-handler1> <resume-handler2>
   (<restart1> <restart2>) (<restart3>) <resume-handler4>) A condition
   handler, or the debugger, can say to resume with a specified
   resume-type keyword.  The innermost resume handler for the
   specified keyword which is applicable to the condition being
   handled will be invoked.  The resume handler is supposed to do a
   throw.  Similarly, a handler or the debugger can resume with a
   specified restart name.  The innermost Common LISP restart which
   has the specified restart name is invoked.

* A resume handler is established with each use of the resume macros, such as,
CONDITION-RESUME, ERROR-RESTART, ERROR-RESTART-LOOP, CATCH-ERROR-RESTART.
  A resume handler is a list of 5 or more elements:
 (CONDITION-NAMES KEYWORD PREDICATE (FORMAT-STRING FORMAT-ARGS...) HANDLER EXTRA-ARGS...).
where
CONDITION-NAMES is a condition name or list of such, or NIL for all conditions.
 This works the same way as CONDITION-NAMES does in EH:*CONDITION-HANDLERS* in
 that it says which conditions this handler is applicable to.
KEYWORD is the resume-type.  This item forms the name of the resume handler.  It
 could be an explicit keyword or could be a cons containing the format-string and
 format-args of a resume macro such as, CATCH-ERROR-RESTART.
PREDICATE is called with one argument, the condition,
 and if the result is non-NIL, this handler is active for this condition.
 PREDICATE can be T, meaning handler is always active if CONDITION-NAMES match.
FORMAT-STRING and FORMAT-ARGS are used to print a message saying what this
 resume handler is intended for.
HANDLER is what to do if the debugger actually says to use this resume handler.
EXTRA-ARGS are passed to HANDLER, after the condition object which is its first arg.
 Any additional values returned from the condition handler or from
 the :PROCEED-ASKING-USER operation (not including the proceed-type)
 are also passed to HANDLER, following the EXTRA-ARGS.
- A resume handler can also be just T, which means don't look past here when
looking for a resume handler for an unhandled SIGNAL on a non-error condition.

* Common LISP restarts are established with use of the Common LISP restart
macros, RESTART-BIND, RESTART-CASE and WITH-SIMPLE-RESTART.  The set of restarts
established in a restart macro is called a restart cluster.  A restart cluster
is a list of restart objects.
Each restart object is a defstruct with 4 slot items:
  RESTART-NAME, RESTART-FUNCTION, RESTART-REPORT-FUNCTION and
RESTART-INTERACTIVE-FUNCTION.
where
RESTART-NAME is the name of the restart.
RESTART-FUNCTION is the function to invoke if the restart is selected in the
 debugger.
RESTART-REPORT-FUNCTION is used to print a description explaining what this
 restart is for.
RESTART-INTERACTIVE-FUNCTION is used to prompt the user for values which are
 passed along to the restart-function to be invoked.
")

(defun condition-name-handled-p (condition-name)
  "Non-nil if there is a handler that might handle condition-name.
   This checks *condition-handlers*, *condition-default-handlers*, and
   *condition-resume-handlers*. Use this to avoid signaling a
   condition that will not be handled; this can often save a lot of
   time."
  ;; Look for a handler for this condition-name; Search
  ;; *condition-handlers*, then *condition-default-handlers* and last
  ;; *condition-resume-handlers*.  For *condition-handlers* and
  ;; *condition-default-handlers*, a handler exists, if either the
  ;; condition name for the handler is nil, or the specified
  ;; condition-name is one of the condition names for this handler;
  ;; For *condition-resume-handlers*, either the handler is t, the
  ;; condition name for the handler is nil, or one of the condition
  ;; names of the handler is the specified condition-name
  (or (dolist (h *condition-handlers*)
	(if (cond ((null (car h)) t)
		  ((symbolp (car h))
		   (eq (car h) condition-name))
		  (t (member condition-name (car h) :test #'eq)))
	    (return (if (eq (cadr h) 'si:condition-case-throw) t 'maybe))))
      (dolist (h *condition-default-handlers*)
	(if (cond ((null (car h)) t)
		  ((symbolp (car h))
		   (eq (car h) condition-name))
		  (t (member condition-name (car h) :test #'eq)))
	    (return (if (eq (cadr h) 'si:condition-case-throw) t 'maybe))))
      (dolist (h *condition-resume-handlers*)
	(if (cond ((eq h t))
		  ((null (car h)) t)
		  ;; Common LISP restarts - mjf/clm 7/20/88
		  ;; Common LISP restart are true for all conditions
		  ;; so if one is found, return t
		  ((and (find-package 'cleh)
			(type-specifier-p 'cleh:restart)
			(typep (car h) 'cleh:restart))
		   (return t))
		  ((symbolp (car h))
		   (eq (car h) condition-name))
		  (t (member condition-name (car h) :test #'eq)))
	    (return t)))))

(defun neq (value1 value2)
  (not (eq value1 value2)))

(defmacro signal-condition (&rest rest)
  `(signal ,@rest))

(defun type-specifier-p (type)
  "Returns T on valid type specifiers, NIL on any other object."
  (sb-kernel::valid-type-specifier-p type))

(defun string-capitalize-words (string &optional (copy-p t) (spaces t))
  "In STRING, turn hyphens to spaces and make each word be capitalized.
If SPACES is NIL, hyphens are not changed.
Copies the original string unless COPY-P is NIL, meaning mung the original."
  (when (and (not copy-p) (stringp string))
    (setf string (copy-sequence string)))
  (nsubstitute-if #\Space
		  (lambda (char)
		    (char= char #\-))  string)
  (format nil " ~:(~a~)" string))



(defun generate-function-property-code (function-name)
  ;;
  (when (consp function-name)
    (let ((name-symbol (car function-name))
	  (property (cadr function-name)))
      `(setf (get ,name-symbol ,property) #',name-symbol))))


;; Taking a stab at creating a replacement for maclisp style defuns, e.g.
;; (defun (:daemon combination-method) (my-function)
;;     ... )
(defmacro maclisp-defun (function-name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn
       (defun ,(if (consp funtion-name) (car function-name) function-name)
	   ,args
	 ,@body)
       ,(generate-function-property-code function-name))))

(defun neq (value-1 value-2)
  (not (eq value-1 value-2)))

(defmacro defprop (symbol value property)
  "make the value of symbol's property property be value."
  `(progn
     (setf (get ',symbol ',property) ',value)
     ',symbol))

;; ;; (LET-IF <COND> ((VAR-1 VAL-1) (VAR-2 VAL-2) ... (VAR-N VAL-N)) &BODY BODY)
;; ;; If <COND> is not nil, binds VAR-I to VAL-I (evaluated) during execution of BODY,
;; ;; otherwise just evaluates BODY.
;; (def let-if (cond &quote var-list &rest body)
;;   "Perform the bindings in var-list only if cond is non-NIL; the
;;    execute the body. Aside from the presence of COND, LET-IF is just
;;    like LET. The variables are always bound as specials if they are
;;    bound; therefore, strictly speaking only variables declared special
;;    should be used."
;;   (progw (and cond var-list)
;;     (eval-body-as-progn body)))


;; (defmacro catch-error-restart ((condition format-string . format-args) &body body)
;;   "Provide a restart for condition if signaled within body.
;;    format-string and format-args are for the debugger to print a
;;    description of what this restart is for, so the user can decide
;;    whether to use it. They are all evaluated when the
;;    catch-error-restart is entered. If the user chooses to go to the
;;    restart we provide, catch-error-restart returns nil as first value
;;    and a non-nil second value. If catch-error-restart is exited
;;    normally, it returns the values of the last form in body."
;;   (let ((tag (gensym)))
;;     `(with-stack-list (,tag ,format-string . ,format-args)
;;        (catch-continuation-if t ,tag
;; 	   #'(lambda () (values nil t))
;; 	   nil
;;          (with-stack-list (,tag ',condition ,tag t
;; 			   ,tag
;; 			   'catch-error-restart-throw ,tag)
;; 	   (with-stack-list* (eh:*condition-resume-handlers*
;; 			      ,tag
;; 			      eh:*condition-resume-handlers*)
;; 	     . ,body))))))





;; (define-condition food-error (error) ())



;;  (define-condition bad-tasting-sundae (food-error)
;;    ((ice-cream :initarg :ice-cream :reader bad-tasting-sundae-ice-cream)
;;     (sauce :initarg :sauce :reader bad-tasting-sundae-sauce)
;;     (topping :initarg :topping :reader bad-tasting-sundae-topping))
;;    (:report (lambda (condition stream)
;;               (format stream "Bad tasting sundae with ~S, ~S, and ~S"
;;                       (bad-tasting-sundae-ice-cream condition)
;;                       (bad-tasting-sundae-sauce condition)
;;                       (bad-tasting-sundae-topping condition)))))





;; (restart-case
;;     (error 'error :report (lambda (condition stream)
;; 			    (declare (ignore condition))
;; 			    (format stream
;; 				    "Leave this form untranslated and proceed with the next top-level form.")))
;;   (my-restart (&optional v)
;;     (declare (ignore v))
;;     (format t "madet it here")))



(defun type-specifier-p (type)
  "Returns T on valid type specifiers, NIL on any other object."
  (sb-kernel::valid-type-specifier-p type))

;;; Conditionally bind some special variables
;;; This breaks down its body using UNZIP-LET-BINDINGS, defined in
;;; /mods/controlmacro/let.lisp.
(defmacro let-if (condition bindings &body body)
  "Perform the bindings in var-list only if cond is non-NIL; the
   execute the body. Aside from the presence of COND, LET-IF is just
   like LET. The variables are always bound as specials if they are
   bound; therefore, strictly speaking only variables declared special
   should be used."
  (multiple-value-bind (vars vals) (unzip-let-bindings bindings)
    (alexandria:once-only (condition)
       `(progv (if ,condition ',vars) (if ,condition (list ,@vals))
	  ,@body))))


;; (LET-IF <COND> ((VAR-1 VAL-1) (VAR-2 VAL-2) ... (VAR-N VAL-N)) &BODY BODY)
;; If <COND> is not nil, binds VAR-I to VAL-I (evaluated) during execution of BODY,
;; otherwise just evaluates BODY.
;; (def let-if (cond &quote var-list &rest body)
;;   "Perform the bindings in var-list only if cond is non-NIL; the
;;    execute the body. Aside from the presence of COND, LET-IF is just
;;    like LET. The variables are always bound as specials if they are
;;    bound; therefore, strictly speaking only variables declared special
;;    should be used."
;;   (progw (and cond var-list)
;;     (eval-body-as-progn body)))
