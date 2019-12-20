;; -*- Mode:Common-Lisp; Package:(G7C TICL); Fonts:(MEDFNT HL12B HL12I MEDFNT MEDFNTB); Patch-file:T; Base:10 -*-

;; Author:  LSS
(defun SI:PROCESS-FLUSH (process)
   "Put a process into 'flushed' state.  The process will remain flushed until it is reset."
  (ticl:send process :flush))

;; Author:  LSS
(defun SI:PROCESS-KILL (process &rest ignore)
  "Sends process a :KILL message.  This is not entirely compatible with Symbolics, 
since we ignore their WITHOUT-ABORTS argument.  For most cases, this function should be fine."
  (ticl:send process :kill))

;; Author:  LSS
(defun SI:PROCESS-INTERRUPT (process function &rest args)
  "Forces the PROCESS to apply function to args.  When FUNCTION returns, PROCESS
continues the interrupted computation.  If PROCESS is waiting, it wakes up, calls FUNCTION, 
then waits again until FUNCTION returns."
  (apply process :interrupt function args))

