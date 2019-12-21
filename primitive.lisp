

(in-package :primitive-zlisp)

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
  (labels (())
    (nlet ((extract-and-remove-options
	    '(:alterant :default-pointer))
	   ;; Check to see if the option was set and its value, if
	   ;; applicable, but don't remove.
	   (query-options
	    '(:conc-name))
	 (partitioned-defstruct-options
	  (partition defstruct-options #'atom #'consp))
	 (alist (cadr partitioned-defstruct-options))
	 (atoms (car partitioned-defstruct-options))
	 (default-pointer (assoc :default-pointer alist))
	 (filtered-alist
	  (remove-if (lambda (item)
		       (and (consp item)
			    (eq (car item) :default-pointer)))
		     alist)))
    (format t "~s~%" default-pointer)
    ;; Reconstitute and return defstruct-options without
    ;; :default-pointer. Return :deault-pointer as a second value, nil
    ;; if not found.
    (values (concatenate 'list filtered-alist atoms)
    	    default-pointer)))

(defun generate-defstruct (name options slot-descriptions)
  `(cl:defstruct (,name ,@options) ,@slot-descriptions))

(defun generate-default-pointer-accessors (slot-descriptions)
)

(defmacro defstruct (name-and-options &body slot-descriptions)
  (nlet ((name-and-options (if (atom name-and-options)
			       (list name-and-options)
			       name-and-options))
	 ((defstruct-name (first name-and-options))
	  (defstruct-options default-pointer alterant
	      (extract-and-remove-default-pointer (rest name-and-options)))))
	`(progn
	   ,(generate-defstruct defstruct-name defstruct-options
				slot-descriptions)
	   ,(generate-default-pointer-accessors slot-descriptions))))

(defun run-defstruct ()
  (dolist (defstruct *defstructs*)
    (format t "~s~%" (macroexpand defstruct))))










;; ;; Extract :dfault-pointer option
;; 	 (default-pointer (getf defstruct-options :default-pointer)))
;;     (remf defstruct-options  :default-pointer)
;;     (values `(list ,defstruct-name ,@defstruct-options) default-pointer)))


;; (cdadr (nth i *defstructs*))






















;; 	(flet

;; ((parse-defstruct (name-and-options slot-descriptions)
;;    ;;the workhorse of defstruct parsing.
;;    ;;returns two values: the defstruct structure, and a list of forms
;;    ;; which will be returned as part of the macroexpansion.
;;    (flet

;; 	 ((find-defstruct-slot (name defstruct)
;; 	    ;;search the slot vector of defstruct for a slot named name.
;; 	    (let ((slot-vector (defstruct-slot-vector defstruct)))
;; 	      (dotimes (i (simple-vector-length slot-vector)
;; 			  (error "no slot named ~s in defstruct ~s"
;; 				 name
;; 				 (defstruct-name defstruct)))
;; 		(let ((this-slot (svref slot-vector i)))
;; 		  (when (eq name (defstruct-slot-name this-slot))
;; 		    (return this-slot)))))))

;; 	 (flet

;; 	   ((check-defstruct-type (def-type)
;; 	      ;;check that type is a legal type for the :type option to
;; 	      ;; defstruct.  in addition, vector types are canonicalized into
;; 	      ;; the form (simple-array element-type (*)).
;; 	      ;; this function guarantees that the type slot in the defstruct
;; 	      ;; will be one of structure, list or
;; 	      ;; (simple-array element-type (*)).
;; 	      (case def-type
;; 		(list def-type)
;; 		(vector '(simple-array t (*)))
;; 		(t (if (and (consp def-type)
;; 			    (eq (first def-type) 'vector)
;; 			    (consp (rest def-type))
;; 			    (null (rest (rest def-type))))
;; 		       `(simple-array ,(second def-type) (*))
;; 		       (progn
;; 			 (cerror
;; 			   "ignore the :type option"
;; 			   "~s is an invalid defstruct :type option"
;; 			   def-type)
;; 			 'structure)))))

;; 	    (make-slot-vector (defstruct
;; 				slot-descriptions
;; 				include-defstruct
;; 				include-slot-descriptions)
;; 	      ;;returns a vector of defstruct-slot structures.
;; 	      ;;if the named slot of defstruct was set (to t), changes
;; 	      ;; it to the index at which it will appear.
;; 	      ;;sets length slot of defstruct to the length to be given to
;; 	      ;; the constructor function.
;; 	      (flet

;; 		((parse-slot-description (slot-description slot)
;; 		   ;;takes a slot description as it appears in a
;; 		   ;; defstruct source, and a slot structure,
;; 		   ;; and fills in only the slots of slot which are specified
;; 		   ;; in the slot description.  if slot-description came
;; 		   ;; from and :include specification, slot will be filled in
;; 		   ;; with the inherited values already, otherwise they will
;; 		   ;; not be initialized
;; 		   ;;notice that a bare nil in the default-init slot
;; 		   ;; indicates that no default was specified.
;; 		   ;; therefore, if nil is actually specified as
;; 		   ;; the default-init, it is converted to (quote nil).
;; 		   (if (symbolp slot-description)
;; 		       (progn
;; 			 (when (constantp slot-description)
;; 			   (cerror "provide a new slot name"
;; 				   "a slot name cannot be a defined constant")
;; 			   (setq slot-description
;; 				 (prompt-and-read :expression "slot name: ")))
;; 			 (setf (defstruct-slot-name slot) slot-description))
;; 		       (destructuring-bind
;; 			 (name &optional (default-init nil default-init-p)
;; 			       &key type (read-only nil read-only-p))
;; 			 slot-description
;; 			 (check-type name
;; 				     (and symbol (not (satisfies constantp)))
;; 				     "a defstruct slot name")
;; 			 (setf (defstruct-slot-name slot) name)
;; 			 (when type
;; 			   (let ((included-type (defstruct-slot-type slot)))
;; 			     (unless (or (null included-type)
;; 					 (subtypep type included-type))
;; 			       (cerror "use the inherited type"
;; 				       "~s is not a subtype of ~s, in slot ~s"
;; 				       type included-type name)
;; 			       (setq type included-type)))
;; 			   (when (and default-init-p
;; 				      (constantp default-init)
;; 				      (type-specifier-p type))
;; 			     (let ((evaluated-default-init
;; 				    (eval default-init)))
;; 			       ;; let #, things through.
;; 			       (unless (or (and (structurep
;; 					       evaluated-default-init)
;; 					      (eq (structure-type
;; 						   evaluated-default-init)
;; 						    'faslescape))
;; 					   (typep evaluated-default-init type))
;; 				 (warn
;; 			       "initial value ~s is not of type ~s, in slot ~s"
;; 				   evaluated-default-init
;; 				   type
;; 				   name))))
;; 			   (setf (defstruct-slot-type slot) type))
;; 			 ;;a plain nil for default-init indicates no default.
;; 			 ;;wrap a quote around explicit ones.
;; 			 (when default-init-p
;; 			   (when (null default-init)
;; 			     (setq default-init ''nil))
;; 			   (setf (defstruct-slot-default-init slot)
;; 				 default-init))
;; 			 (when read-only-p
;; 			   (when (and (not read-only)
;; 				      (defstruct-slot-read-only slot))
;; 			     (cerror "leave the slot read-only"
;; 				     "slot ~s cannot be made writable" name)
;; 			     (setq read-only t))
;; 			   (setf (defstruct-slot-read-only slot)
;; 				 read-only))))))

;; 		;;make-slot-vector
;; 		(let* ((conc-name (defstruct-conc-name defstruct))
;; 		       (local-starting-slot 0)
;; 		       (include-slot-vector nil)
;; 		       (include-length 0))
;; 		  (when include-defstruct
;; 		    (setq include-slot-vector
;; 			  (defstruct-slot-vector include-defstruct))
;; 		    (setq include-length
;; 			  (simple-vector-length include-slot-vector))
;; 		    (setq local-starting-slot
;; 			  ;;take the index of the last included slot
;; 			  ;; and add one.  watch out for empty included
;; 			  ;; vectors!!
;; 			  (if (=& include-length 0)
;; 			      0
;; 			      (1+& (defstruct-slot-index
;; 				     (svref include-slot-vector
;; 					    (1-& include-length)))))))
;; 		  (let* ((slot-vector-length
;; 			   (+& include-length (list-length slot-descriptions)))
;; 			 (slot-vector (new-simple-vector slot-vector-length)))
;; 		    ;;if there was an :include option, copy all the slots from
;; 		    ;; the included slot vector and change any options which
;; 		    ;; were overridden in the :include specification
;; 		    (when include-slot-vector
;; 		      ;;first, check all the included slot specs to make sure
;; 		      ;; they all refer to slots in the included structure,
;; 		      ;; and that they are all different.
;; 		      (do ((include-slot-names '())
;; 			   (slot-desc-tail include-slot-descriptions
;; 					   (rest slot-desc-tail)))
;; 			  ((atom slot-desc-tail)
;; 			   (unless (null slot-desc-tail)
;; 			     (cerror
;; 			       "ignore the tail of the list"
;; 			       "malformed include slot description list ~s"
;; 			       include-slot-descriptions)))
;; 			(let* ((slot-description (first slot-desc-tail))
;; 			       (name (if (consp slot-description)
;; 					 (first slot-description)
;; 					 slot-description)))
;; 			  (find-defstruct-slot name include-defstruct)
;; 			  (if (memq name include-slot-names)
;; 			      (cerror
;; 				"ignore this slot description"
;; 				"duplicate include slot description ~s"
;; 				slot-description)
;; 			      (push name include-slot-names))))
;; 		      (dotimes (i include-length)
;; 			(let* ((included-slot (svref include-slot-vector i))
;; 			       (new-slot (copy-defstruct-slot included-slot))
;; 			       (new-slot-description
;; 				 (assq (defstruct-slot-name included-slot)
;; 				       include-slot-descriptions)))
;; 			  (when new-slot-description
;; 			    (parse-slot-description new-slot-description
;; 						    new-slot))
;; 			  (setf (svref slot-vector i) new-slot))))
;; 		    (let ((initial-offset (defstruct-initial-offset defstruct))
;; 			  (named (defstruct-named defstruct)))
;; 		      (when initial-offset
;; 			(incf& local-starting-slot initial-offset))
;; 		      (when named
;; 			(setf (defstruct-named defstruct) local-starting-slot)
;; 			(incf& local-starting-slot)))
;; 		    (let ((i include-length)
;; 			  (index local-starting-slot))
;; 		      (do ((slot-desc-tail slot-descriptions
;; 					   (rest slot-desc-tail)))
;; 			  ((atom slot-desc-tail)
;; 			   (unless (null slot-desc-tail)
;; 			     (cerror
;; 			       "ignore the tail of the list"
;; 			       "malformed include slot description list ~s"
;; 			       include-slot-descriptions)))
;; 			(let ((slot-description (first slot-desc-tail))
;; 			      (slot (make-defstruct-slot :default-init nil
;; 							 :type nil
;; 							 :read-only nil
;; 							 :index index)))
;; 			  (parse-slot-description slot-description slot)
;; 			  (setf (svref slot-vector i) slot))
;; 			(incf& i)
;; 			(incf& index))
;; 		      (setf (defstruct-length defstruct) index))
;; 		    ;;check for slot name conflicts and set all the
;; 		    ;; accessor names
;; 		    (dotimes (i slot-vector-length)
;; 		      (let* ((slot (svref slot-vector i))
;; 			     (name (defstruct-slot-name slot))
;; 			     (string (symbol-name name)))
;; 			;;check for name conflicts
;; 			(block finished-checking
;; 			  (loop
;; 			    (block check-again
;; 			      (dotimes (j i (return-from finished-checking))
;; 				(when (string= string
;; 					       (symbol-name
;; 						 (defstruct-slot-name
;; 						   (svref slot-vector j))))
;; 				  (cerror "supply a new slot name"
;; 					  "there is already a slot named ~s"
;; 					  name)
;; 				  (setq name (prompt-and-read
;; 					       :expression
;; 					       "enter a new slot name: "))
;; 				  (check-type name symbol)
;; 				  (setf (defstruct-slot-name slot) name)
;; 				  (setq string (symbol-name name))
;; 				  (return-from check-again))))))
;; 			;;set the accessor name
;; 			(setf (defstruct-slot-accessor slot)
;; 			      (append-symbols conc-name name))))
;; 		    slot-vector))))

;; 	    (make-constructors (defstruct constructor-options)
;; 	      ;;constructor-options is a list of the tails of the
;; 	      ;; :constructor options supplied to defstruct.  this function
;; 	      ;; returns a list of forms (generally defsubsts) which
;; 	      ;; will define the constructors specified.  if no options are
;; 	      ;; specified, just the standard keyword constructor is made.
;; 	      (flet

;; 		((make-vector-name-setters (defstruct variable)
;; 		   ;;returns a list of forms which will set the name slots
;; 		   ;; of variable.  variable will be a vector-type structure.
;; 		   (let ((name-setters '())
;; 			 (named (defstruct-named defstruct)))
;; 		     (loop
;; 		       (when named
;; 			 (push `(setf (aref ,variable ,named)
;; 				      ',(defstruct-name defstruct))
;; 			       name-setters))
;; 		       (setq defstruct (defstruct-include defstruct))
;; 		       (unless defstruct
;; 			 (return name-setters))
;; 		       (setq defstruct (get-defstruct-check defstruct))
;; 		       (setq named (defstruct-named defstruct)))))

;; 		 (make-constructor-slot-setters (defstruct
;; 						  arglist
;; 						  structure-temp
;; 						  setter-conser
;; 						  boa-p
;; 						  no-defaults)
;; 		   ;;returns a list of setting forms to set the
;; 		   ;; slots of a new structure to either the
;; 		   ;; supplied initial value or the default initial
;; 		   ;; value.  if neither is specified, no setting
;; 		   ;; will be done.  this is done by using supplied-p
;; 		   ;; arguments for those slots without defaults.
;; 		   (do* ((slot-vector (defstruct-slot-vector defstruct))
;; 			 (length (simple-vector-length slot-vector))
;; 			 (type (defstruct-type defstruct))
;; 			 (setting-forms '())
;; 			 (name (defstruct-name defstruct))
;; 			 (i 0 (+& i 1)))
;; 			((>=& i length)
;; 			 (list-nreverse setting-forms))
;; 		     (let* ((slot (svref slot-vector i))
;; 			    (index (defstruct-slot-index slot))
;; 			    (slot-name (defstruct-slot-name slot)))
;; 		       (if boa-p
;; 			   (let* ((user-supplied (memq slot-name arglist))
;; 				  (form-to-use
;; 				    (if user-supplied
;; 					slot-name
;; 					(or (defstruct-slot-default-init slot)
;; 					    (if (and defstruct-nil-default
;; 						     (or (atom type)
;; 							 (eq (second type) t)))
;; 						''nil
;; 						nil)))))
;; 			     (when form-to-use
;; 			       (let ((setting-form
;; 				       (funcall setter-conser
;; 						structure-temp
;; 						index
;; 						name
;; 						form-to-use))
;; 				     (no-default
;; 				       (if user-supplied
;; 					   (assq slot-name no-defaults)
;; 					   nil)))
;; 				 (when no-default
;; 				   (setq setting-form
;; 					 `(if ,(cdr no-default)
;; 					      ,setting-form)))
;; 				 (push setting-form setting-forms))))
;; 			   (let ((setting-form
;; 				   (funcall setter-conser
;; 					    structure-temp
;; 					    index
;; 					    name
;; 					    slot-name))
;; 				 (arg-tail (rest (rest (first arglist)))))
;; 			     (when arg-tail
;; 			       ;;supplied-p parameter
;; 			       (setq setting-form
;; 				     `(if ,(first arg-tail) ,setting-form)))
;; 			     (push setting-form setting-forms)
;; 			     (setq arglist (rest arglist)))))))

;; 		 (make-list-arguments (defstruct boa-p boa-slots)
;; 		   ;;return a list of forms which will be arguments
;; 		   ;; in a call to list, to create a structure of
;; 		   ;; type list.  if boa-p is true, boa-slots is a
;; 		   ;; list of the slots which are supplied by the user.
;; 		   (let* ((arguments '())
;; 			  (ds defstruct)
;; 			  (slot-vector (defstruct-slot-vector defstruct))
;; 			  (ending-index (simple-vector-length slot-vector))
;; 			  (include (defstruct-include defstruct))
;; 			  (isv nil)
;; 			  (starting-index 0))
;; 		     (loop
;; 		       ;;arguments are pushed in reverse order
;; 		       (if include
;; 			   (progn
;; 			     (setq include (get-defstruct-check include))
;; 			     (setq isv (defstruct-slot-vector include))
;; 			     (setq starting-index
;; 				   (simple-vector-length isv)))
;; 			   (setq starting-index 0))
;; 		       ;;supply all the slot names at this
;; 		       ;; :include level, in reverse order
;; 		       (if boa-p
;; 			   (do ((i (-& ending-index 1) (-& i 1)))
;; 			       ((<& i starting-index))
;; 			     (let* ((slot (svref slot-vector i))
;; 				    (name (defstruct-slot-name slot))
;; 				    (form-to-use
;; 				      (if (memq name boa-slots)
;; 					  name
;; 					  (defstruct-slot-default-init slot))))
;; 			       (push form-to-use arguments)))
;; 			   (do ((i (-& ending-index 1) (-& i 1)))
;; 			       ((<& i starting-index))
;; 			     (push (defstruct-slot-name (svref slot-vector i))
;; 				   arguments)))
;; 		       ;;fill in the name slot
;; 		       (when (defstruct-named ds)
;; 			 (push `',(defstruct-name ds) arguments))
;; 		       ;;supply nils for any initial offset
;; 		       (let ((initial-offset
;; 			       (defstruct-initial-offset ds)))
;; 			 (when initial-offset
;; 			   (dotimes (i initial-offset)
;; 			     (push nil arguments))))
;; 		       (if include
;; 			   (progn
;; 			     (setq ds include)
;; 			     (setq include (defstruct-include ds))
;; 			     (setq ending-index starting-index))
;; 			   (return arguments))))))

;; 		(flet

;; 		  ((make-keyword-constructor
;; 		      (defstruct constructor-name)
;; 		     ;;make a standard keyword-style constructor defsubst
;; 		     ;; named constructor-name.  also sets the constructor
;; 		     ;; slot of defstruct to constructor-name.
;; 		     (flet

;; 		       ((make-keyword-arglist (defstruct)
;; 			  (let ((slot-vector (defstruct-slot-vector defstruct))
;; 				(arglist '())
;; 				(type (defstruct-type defstruct)))
;; 			    (dotimes (i (simple-vector-length slot-vector))
;; 			      (let* ((slot (svref slot-vector i))
;; 				     (name (defstruct-slot-name slot))
;; 				     (default-init
;; 				       (defstruct-slot-default-init slot)))
;; 				;;supplied-p is used unless there is a
;; 				;;default-init or the type is list
;; 				(push `(,name ,default-init
;; 					,@(if (or default-init
;; 						  (eq type 'list)
;; 						  (and defstruct-nil-default
;; 						       (or (atom type)
;; 							   (eq (second type)
;; 							       t))))
;; 					      `()
;; 					      `(,(gensym))))
;; 				      arglist)))
;; 			    (list-nreverse arglist))))

;; 		       ;;make-keyword-constructor
;; 		       (let* ((name (defstruct-name defstruct))
;; 			      (structure-temp (gensym))
;; 			      (keyword-arglist
;; 				(make-keyword-arglist defstruct)))
;; 			 (declare (inline make-keyword-arglist))
;; 			 (setf (defstruct-constructor defstruct)
;; 			       constructor-name)
;; 			 `(defsubst ,constructor-name (&key ,@keyword-arglist)
;; 			    ,(let ((type (defstruct-type defstruct)))
;; 			       (case type
;; 				 (structure
;; 				   `(let ((,structure-temp
;; 					   (new-structure
;; 					     ,(defstruct-length defstruct)
;; 					     ',name)))
;; 				      ,.(make-constructor-slot-setters
;; 					  defstruct
;; 					  keyword-arglist
;; 					  structure-temp
;; 					  #'(lambda (struct index name value)
;; 					      `(set-structure-ref ,struct
;; 								  ,index
;; 								  ',name
;; 								  ,value))
;; 					  nil
;; 					  nil)
;; 				      ,structure-temp))
;; 				 (list
;; 				   `(list ,@(make-list-arguments
;; 					      defstruct
;; 					      nil
;; 					      nil)))
;; 				 (t
;; 				  (let ((element-type (second type)))
;; 				    (if (eq element-type 't)
;; 					`(vector ,@(make-list-arguments
;; 						     defstruct
;; 						     nil
;; 						     nil))
;; 					`(let ((,structure-temp
;; 						(make-array
;; 						  ,(defstruct-length
;; 						       defstruct)
;; 						  :element-type
;; 						  ',element-type)))
;; 					   (declare (type ,type
;; 							  ,structure-temp))
;; 					   ,.(make-vector-name-setters
;; 					       defstruct
;; 					       structure-temp)
;; 					   ,.(make-constructor-slot-setters
;; 					       defstruct
;; 					       keyword-arglist
;; 					       structure-temp
;; 					       #'(lambda (struct index name
;; 								 value)
;; 						   (declare (ignore name))
;; 						   name
;; 						   `(setf (aref ,struct ,index)
;; 							  ,value))
;; 					       nil
;; 					       nil)
;; 					   ,structure-temp))))))))))

;; 		   (make-boa-constructor (defstruct constructor-name arglist)
;; 		     (let ((type (defstruct-type defstruct))
;; 			   (fixed-arglist '())
;; 			   (slots-in-arglist '())
;; 			   ;;a-list of (name . supplied-p)
;; 			   (no-defaults '()))
;; 		       ;; find the slots in the arglist
;; 		       ;;  and hack optionals with no explicit
;; 		       ;;  defaults to inherit the defstruct defaults.
;; 		       ;; we don't need to type check the argument list
;; 		       ;;  because it gets handed to defun.
;; 		       (let ((arg-kind nil))
;; 			 (dolist (arg arglist)
;; 			   (if (atom arg)
;; 			       (if
;; 				 (memq arg '(&optional &rest &aux))
;; 				 (setq arg-kind arg)
;; 				 ;; optional, but no new default
;; 				 (progn
;; 				   (push arg slots-in-arglist)
;; 				   (if
;; 				     (eq arg-kind '&optional)
;; 				     (let*
;; 				       ((default-init
;; 					  (defstruct-slot-default-init
;; 					    (find-defstruct-slot
;; 					      arg
;; 					      defstruct)))
;; 					(supplied-p
;; 					  (if (or default-init
;; 						  (eq type 'list)
;; 						  (and defstruct-nil-default
;; 						       (or (atom type)
;; 							   (eq (second type)
;; 							       t))))
;; 					      nil
;; 					      (let ((temp (gensym)))
;; 						(push
;; 						  (cons arg temp)
;; 						  no-defaults)
;; 						temp))))
;; 				       (setq arg
;; 					     `(,arg ,default-init
;; 					       ,@(if supplied-p
;; 						     `(,supplied-p)
;; 						     `()))))
;; 				     ;;checking for existence
;; 				     ;; of argument
;; 				     (find-defstruct-slot arg defstruct))))
;; 			       (let ((name (first arg)))
;; 				 ;;checking for existence of argument
;; 				 (find-defstruct-slot name defstruct)
;; 				 ;;user supplied default.
;; 				 (push name slots-in-arglist)))
;; 			   (push arg fixed-arglist)))
;; 		       (setq fixed-arglist (list-nreverse fixed-arglist))
;; 		       (let ((structure-temp (gensym)))
;; 			 `(defsubst ,constructor-name ,fixed-arglist
;; 			    ,(case type
;; 			       (structure
;; 				 `(let ((,structure-temp
;; 					 (new-structure
;; 					   ,(defstruct-length defstruct)
;; 					   ',(defstruct-name
;; 					       defstruct))))
;; 				    ,.(make-constructor-slot-setters
;; 					defstruct
;; 					slots-in-arglist
;; 					structure-temp
;; 					#'(lambda (struct
;; 						   index
;; 						   name
;; 						   value)
;; 					    `(set-structure-ref ,struct
;; 								,index
;; 								',name
;; 								,value))
;; 					t
;; 					no-defaults)
;; 				    ,structure-temp))
;; 			       (list
;; 				 `(list ,@(make-list-arguments
;; 					    defstruct
;; 					    t
;; 					    slots-in-arglist)))
;; 			       (t
;; 				(if (eq (second type) 't)
;; 				    `(vector ,@(make-list-arguments
;; 						 defstruct
;; 						 t
;; 						 slots-in-arglist))
;; 				    `(let ((,structure-temp
;; 					    `(make-array
;; 					       ,(defstruct-length defstruct)
;; 					       :element-type
;; 					       ',(second type))))
;; 				    (declare
;; 				      (type ,type ,structure-temp))
;; 				    ,.(make-vector-name-setters
;; 				        defstruct
;; 				        structure-temp)
;; 				    ,.(make-constructor-slot-setters
;; 					defstruct
;; 					slots-in-arglist
;; 					structure-temp
;; 					#'(lambda (struct
;; 						   index
;; 						   name
;; 						   value)
;; 					    (declare (ignore name))
;; 					    name
;; 					    `(setf (aref ,struct ,index)
;; 						   ,value))
;; 					t
;; 					no-defaults)
;; 				    ,structure-temp)))))))))

;; 		  ;;make-constructors
;; 		  (let ((constructors '())
;; 			(constructor-names '()))
;; 		    (declare (inline make-keyword-constructor
;; 				     make-boa-constructor))
;; 		    (when (null constructor-options)
;; 		      (setq constructor-options '(())))
;; 		    (dolist (option constructor-options)
;; 		      (check-type option list)
;; 		      (when (null option)
;; 			(setq option (list (append-symbols
;; 					     'make-
;; 					     (defstruct-name defstruct)))))
;; 		      (let ((constructor-name (first option))
;; 			    (tail (rest option)))
;; 			(when constructor-name
;; 			  (check-type constructor-name symbol)
;; 			  (push (if (consp tail)
;; 				    (make-boa-constructor
;; 				      defstruct
;; 				      constructor-name
;; 				      (first tail))
;; 				    (make-keyword-constructor
;; 				      defstruct
;; 				      constructor-name))
;; 				constructors)
;; 			  (push constructor-name constructor-names))))
;; 		    (setf (defstruct-constructors defstruct)
;; 			  (coerce constructor-names 'simple-vector))
;; 		    constructors))))

;; 	    (make-alternating-keyword-options (options-list)
;; 	      ;;create an alternating keyword and value list to pass to
;; 	      ;; destructuring-bind.  eliminate duplicate options except
;; 	      ;; for :constructor.
;; 	      (flet

;; 		((option-already-given-error (option)
;; 		   (cerror "ignore this option"
;; 			   "~s option has already been specified"
;; 			   option)))

;; 		;;make-alternating-keyword-options
;; 		(let ((constructors '())
;; 		      (options-given '())
;; 		      (alternating-keyword-options '()))
;; 		  (declare (inline option-already-given-error))
;; 		  (dolist (option options-list)
;; 		    (if (atom option)
;; 			(if (eq option :constructor)
;; 			    (push '() constructors)
;; 			    (progn
;; 			      (if (memq option options-given)
;; 				  (option-already-given-error option)
;; 				  (progn
;; 				    (push option options-given)
;; 				    (push '() alternating-keyword-options)
;; 				    (push option
;; 					  alternating-keyword-options)))))
;; 			(let ((1st (first option)))
;; 			  (if (eq 1st :constructor)
;; 			      (push (rest option) constructors)
;; 			      (progn
;; 				(if (memq 1st options-given)
;; 				    (option-already-given-error option)
;; 				    (progn
;; 				      (push 1st options-given)
;; 				      (push (rest option)
;; 					    alternating-keyword-options)
;; 				      (push 1st
;; 					    alternating-keyword-options))))))))
;; 		  (push constructors alternating-keyword-options)
;; 		  (push :constructor alternating-keyword-options)
;; 		  alternating-keyword-options))))

;; 	   ;;parse-defstruct
;; 	   (let ((defstruct (make-defstruct))
;; 		 (forms-constructed-now '()))
;; 	     (declare (inline check-defstruct-type make-slot-vector
;; 			      make-alternating-keyword-options
;; 			      make-constructors))
;; 	     (cond
;; 	       ((and (symbolp name-and-options) (not (null name-and-options)))
;; 		(setf (defstruct-name defstruct) name-and-options)
;; 		(setq name-and-options '()))
;; 	       (t
;; 		(check-type name-and-options cons
;; 			    "a defstruct name-and-options list")
;; 		(check-type (first name-and-options) symbol "a defstruct name")
;; 		(setf (defstruct-name defstruct) (first name-and-options))
;; 		(setq name-and-options (rest name-and-options))))
;; 	     (destructuring-bind
;; 	       (&key conc-name copier predicate (include nil include-p)
;; 		     constructor (print-function nil print-function-p)
;; 		     (type nil type-p) (named nil named-p) initial-offset)
;; 	       (make-alternating-keyword-options name-and-options)
;; 	       ;;record the value of *package* which is used to create
;; 	       ;; symbols.
;; 	       (setf (defstruct-package defstruct)
;; 		     (package-name *package*))
;; 	       (setf (defstruct-conc-name defstruct)
;; 		     (if (consp conc-name)
;; 			 ;;if (:conc-name nil) was specified, use the zero-
;; 			 ;; length symbol as the conc-name.  if a string
;; 			 ;; was specified, intern it.
;; 			 (let ((name (first conc-name)))
;; 			   (cond ((null name) '||)
;; 				 ((symbolp name) name)
;; 				 ((stringp name) (intern name))
;; 				 (t (check-type (first conc-name) symbol)
;; 				    (or (first conc-name) '||))))
;; 			 (append-symbols (defstruct-name defstruct) '-)))
;; 	       (setf (defstruct-copier defstruct)
;; 		     (if (consp copier)
;; 			 (progn (check-type (first copier) symbol)
;; 				(first copier))
;; 			 (append-symbols 'copy- (defstruct-name defstruct))))
;; 	       (setf (defstruct-type defstruct)
;; 		     (if type-p
;; 			 (if (and (consp type)
;; 				  (null (rest type)))
;; 			     (check-defstruct-type (first type))
;; 			     (progn (cerror ":type must name a valid type"
;; 					    "ignore the :type option")
;; 				    'structure))
;; 			 'structure))
;; 	       (when (and (eq (defstruct-type defstruct) 'structure)
;; 			  (or named-p initial-offset))
;; 		 (cerror
;; 		   "ignore the :named and :initial-offset options"
;; 		   ":named and :initial-offset may only be used with :type")
;; 		 (setq named-p nil)
;; 		 (setq initial-offset nil))
;; 	       (when named
;; 		 (cerror "ignore the argument to :named"
;; 			 ":named should not have an argument"))
;; 	       (when named-p
;; 		 (let ((type (defstruct-type defstruct)))
;; 		   (when (consp type)
;; 		     ;;it must be (simple-array element-type (*))
;; 		     (let ((element-type (second type)))
;; 		       (unless (subtypep 'symbol element-type)
;; 			 (cerror
;; 			   "ignore the :named option"
;; 			   ":named cannot be used with ~s structures" type)
;; 			 (setq named-p nil))))))
;; 	       (setf (defstruct-named defstruct) named-p)
;; 	       (when initial-offset
;; 		 (unless (and (consp initial-offset)
;; 			      (fixnump (setq initial-offset
;; 					     (first initial-offset)))
;; 			      (>=& initial-offset 0))
;; 		   (cerror
;; 		     "ignore the :initial-offset argument"
;; 		     ":initial-offset must have a positive integer argument")
;; 		   (setq initial-offset nil)))
;; 	       (setf (defstruct-initial-offset defstruct) initial-offset)
;; 	       (let ((predicate-allowed
;; 		       (or (eq (defstruct-type defstruct) 'structure)
;; 			   named-p)))
;; 		 (when (and (consp predicate)
;; 			    (not predicate-allowed)
;; 			    (not (eq (first predicate) nil)))
;; 		   (cerror
;; 		     "ignore the :predicate option"
;; 		     ":predicate may only be used with :named or without :type")
;; 		   (setq predicate nil))
;; 		 (setf (defstruct-predicate defstruct)
;; 		       (if (consp predicate)
;; 			   (progn (check-type (first predicate) symbol)
;; 				  (first predicate))
;; 			   (and predicate-allowed
;; 				(append-symbols (defstruct-name defstruct)
;; 						'-p)))))
;; 	       (when print-function-p
;; 		 (cond ((atom print-function)
;; 			(cerror "ignore the :print-function argument"
;; 				":print-function must have an argument")
;; 			(setq print-function nil))
;; 		       ((not (eq (defstruct-type defstruct) 'structure))
;; 			(cerror "ignore the :print-function option"
;; 				":print-function may not be used with :type")
;; 			(setq print-function nil))
;; 		       (t
;; 			(let ((print-function-name (first print-function)))
;; 			  (if (and (consp print-function-name)
;; 				   (eq (first print-function-name) 'lambda))
;; 			      (let ((dummy-name
;; 				      (make-symbol
;; 					(simple-string-concatenate
;; 					  (symbol-name
;; 					    (defstruct-name defstruct))
;; 					  "-print-function"))))
;; 				(push `(defun ,dummy-name
;; 					      ,@(rest print-function-name))
;; 				      forms-constructed-now)
;; 				(setq print-function-name dummy-name))
;; 			      (check-type print-function-name symbol))
;; 			  (setq print-function print-function-name)))))
;; 	       (setf (defstruct-print-function defstruct) print-function)
;; 	       (when (stringp (first slot-descriptions))
;; 		 (setf (defstruct-documentation defstruct)
;; 		       (first slot-descriptions))
;; 		 (setq slot-descriptions (rest slot-descriptions)))
;; 	       (let ((include-defstruct nil)
;; 		     (include-slot-descriptions '()))
;; 		 (when include-p
;; 		   (unless (consp include)
;; 		     (cerror "ignore the :include option"
;; 			     ":include must have an argument")))
;; 		 (when (consp include)
;; 		   (check-type (first include) symbol)
;; 		   (setq include-slot-descriptions (rest include))
;; 		   (setq include (first include))
;; 		   (setq include-defstruct (get-defstruct include))
;; 		   (unless (and include-defstruct
;; 				(equal (defstruct-type include-defstruct)
;; 				       (defstruct-type defstruct)))
;; 		     (cerror
;; 		       "ignore the :include option"
;; 		       ":include must name a defstruct of the same type")
;; 		     (setq include-defstruct nil)
;; 		     (setq include-slot-descriptions '())))
;; 		 (setf (defstruct-include defstruct) include)
;; 		 (setf (defstruct-slot-vector defstruct)
;; 		       (make-slot-vector defstruct
;; 					 slot-descriptions
;; 					 include-defstruct
;; 					 include-slot-descriptions))
;; 		 (setq forms-constructed-now
;; 		       (nconc (make-constructors defstruct
;; 						 (list-nreverse constructor))
;; 			      forms-constructed-now))
;; 		 (values defstruct
;; 			 (list-nreverse forms-constructed-now)))))))))

;; ;;defstruct
;; (locally
;;   (declare (inline parse-defstruct))
;;   (multiple-value-bind (defstruct forms-constructed-now)
;; 	  (parse-defstruct name-and-options slot-descriptions)
;; 	`(progn (define-structure ',defstruct)
;; 		,@forms-constructed-now
;; 		',(defstruct-name defstruct))))))
