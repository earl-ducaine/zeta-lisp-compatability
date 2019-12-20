;;; -*- Mode:Common-Lisp; Package:SYSTEM; Base:10 -*-

;;; **********************************************************************
;;; Copyright (c) 1990 Stanford University.
;;; Copyright is held by Stanford University except where code has been
;;; modified from TI source code.  In these cases TI code is marked with
;;; a suitable comment.  Where functionality implemented herein replicates
;;; similarly named functionality on Symbolics machines, this code was
;;; developed solely from the interface specification in the documentation
;;; or through guesswork, never by examination of Symbolics source code.

;;; All Stanford Copyright code is in the public domain.  This code may be
;;; distributed and used without restriction as long as this copyright
;;; notice is included and no fee is charged.  This can be thought of as
;;; being equivalent to the Free Software Foundation's Copyleft policy.

;;; TI source code may only be distributed to users who hold valid TI
;;; software licenses.
;;; **********************************************************************

(defwhopper (w:live-listener-synchronous-UCL-mixin :tyi) (&rest args)
  (let ((result (lexpr-continue-whopper args)))
       (ll-tyi-internal result)
  )
)

(defwhopper (w:live-listener-synchronous-UCL-mixin :any-tyi) (&rest args)
  (let ((result (lexpr-continue-whopper args)))
       (ll-tyi-internal result)
  )
)

