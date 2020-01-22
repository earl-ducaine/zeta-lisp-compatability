

(in-package :zwei)


;;; 988

;;; Return an interval stream outputing at BP
(defun interval-stream-into-bp (bp &optional hack-fonts)
  "Return a stream that outputs text at BP.
   hack-fonts = t means return epsilon prefixes if the text contains
   multiple fonts. hack-fonts = :read-char means return characters
   with fonts if the text contains them."
  ;; Patch 94.142   ddd/gsl 2/28/84.
  (interval-stream bp bp t hack-fonts))
