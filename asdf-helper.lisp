

(in-package :asdf-helper)

;; The system of the component needs to have been loaded, keeping in
;; mind that systems are themselves components.
(defun get-system-components (component-name)
  (asdf:load-system component-name)
  (asdf:required-components :hemlock.base))


;; (gmap:gmap (:list :filterp #'cl-source-file-p) #'+ (:index 0 6) (:constant 17))


(defun system-files (system &rest source-file-types)
  (declare (ignore source-file-types))
  (labels ((get-component-absolute-path (slot)
	     (slot-value slot 'asdf/component::absolute-pathname)))

    (mapcar #'get-component-absolute-path
	    (gmap:gmap (:list :filterp #'cl-source-file-p)
		       #'identity
		       (:list (get-system-components system))))))


(defun run-system-files ()
  (system-files :hemlock.base))



(defun cl-source-file-p (object)
  (typep object 'ASDF/LISP-ACTION:CL-SOURCE-FILE))

;; (gmap:gmap
;;  (:list :filterp (lambda (item)
;; 		   (typep item 'ASDF/LISP-ACTION:CL-SOURCE-FILE)))
;;  #'identity
;;  (:list)
;;  (get-system-components :hemlock.base))


;; (defun system-files (system-name)
;;   get-system-components


(defun get-system-component-file-path (component)
  (slot-value component 'asdf/component::absolute-pathname))

;; (defun fixnump (number)
;;   (typep number 'fixnum))

;; (defun test-retries ()
;;   )

;; (define-condition food-error (error) ()
;;   (:report (lambda (condition stream)
;;              (format stream "Bad tasting sundae with ~S, ~S, and ~S"
;; 		     'item1
;; 		     'item2
;; 		     'item3))))

;; (define-condition bad-tasting-sundae (food-error)
;;   ((ice-cream :initarg :ice-cream :reader bad-tasting-sundae-ice-cream)
;;    (sauce :initarg :sauce :reader bad-tasting-sundae-sauce)
;;    (topping :initarg :topping :reader bad-tasting-sundae-topping))
;;   (:report (lambda (condition stream)
;;              (format stream "Bad tasting sundae with ~S, ~S, and ~S"
;;                      (bad-tasting-sundae-ice-cream condition)
;;                      (bad-tasting-sundae-sauce condition)
;;                      (bad-tasting-sundae-topping condition)))))


;; (defun compute-fixnum-power-of-2 (x)
;;   (with-simple-restart (nil "Give up on computing 2^~D." x)
;;     (let ((result 1))
;;       (dotimes (i x result)
;;         (setq result (* 2 result))
;;         (unless (fixnump result)
;; 	  (error 'food-error
;; 		 :report "bad icecream"))))))


;;          (error "Power of 2 is too large ~A." x))))))
