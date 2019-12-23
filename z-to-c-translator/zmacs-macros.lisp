



(in-package :zwei)

(defun str (&rest rest)
  (apply #'concatenate 'string rest))

(defvar *command-alist* ()
  "Alist of all ZWEI command names (strings) versus the commands themselves
   (symbols).")

(defun command-define (command doc ignore)
  (declare (ignore ignore))
  (cond ((stringp doc)
	 (setf (documentation command 'function) doc))
	((or (symbolp doc)
	     (and (not (atom doc))
		  (member (car doc) '(function lambda) :test #'eq)))
	 (setf (get command 'documentation-function) doc))
	(t
	 (ferror nil "The command ~s has invalid self-documentation ~s"
		 command doc)))
  (let* ((name (make-command-name command))
	 (aentry (assoc name *command-alist* :test 'equalp)))
    (setf (get command 'command-name) name)
    (if aentry
	(unless (eq command (cdr aentry))
	  (when (fquery nil (str "the command name ~s is currently set up "
				 "to call ~s; redefine it to call ~s? ")
				 name (cdr aentry) command)
	    (setf (cdr aentry) command)))
	(push (cons name command) *command-alist*))))


;;; Defines a command.  Form is:
;;; (DEFCOM COM-foo "Documentation." OPTIONS-LIST . BODY)
;;; Note: unlike EINE, there is no lambda-list.
(defmacro defcom (fn doc options &body def)
  "options are:(km)   -- this command always preserves mark.
               (sm)   -- set mark. there will be a region after this command
               (nm)   -- reset mark. there will be no region after this command
               (push) -- push point onto the point-pdl
               (r)    -- frob recentering fraction for forward move
               (-r)   -- frob recentering fraction for backward move."
  `(progn
     'compile
     (command-define ',fn ',doc ',options)
     (defun ,fn ()
       ,@(process-command-options options)
       ,@def)))
