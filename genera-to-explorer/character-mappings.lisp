;;; -*- Mode:Common-Lisp; Package:System; Base:10 -*-

;;; **********************************************************************
;;; Copyright (c) 1990 Stanford University.
;;; This code was written by members of the Large Knowledge Base project.
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

;-------------------------------------------------------------------------------

;;; Character patches.

(defun add-pseudo-character-mapping (char-name map-to)
  (let ((item (cons char-name (char-int map-to))))
       (if (member item si:xr-special-character-names :Test #'equalp)
	   nil
	   (nconc si:xr-special-character-names (list item))
       )
  )
)

(add-pseudo-character-mapping :Meta-Control-Shift-Mouse-L #\Meta-Mouse-L)
;;;Edited by James Rice            6 Jun 90  15:42
(add-pseudo-character-mapping :Control-Shift-Mouse-M #\Meta-Mouse-M)
(add-pseudo-character-mapping :Shift-Mouse-R #\Mouse-R)
(add-pseudo-character-mapping :Shift-Mouse-M #\Mouse-M)
(add-pseudo-character-mapping :Mouse-Right #\Mouse-R)
(add-pseudo-character-mapping :Mouse-Left #\Mouse-L)
(add-pseudo-character-mapping :Mouse-Middle #\Mouse-M)
(add-pseudo-character-mapping :Complete #\Escape)
;;;Edited by James Rice            19 Apr 90  12:32
(add-pseudo-character-mapping :Triangle #\F1)

;-------------------------------------------------------------------------------
