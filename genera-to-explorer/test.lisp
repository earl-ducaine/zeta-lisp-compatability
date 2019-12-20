


(net:define-server :foo (:medium :byte-stream
				 :who-line t
				 :host host
				 :stream (recv-stream :accept-p nil :characters t))
  (let ((line (read-line recv-stream)))
    (beep)
       (format tv:selected-window "Yes I got your message locally.  It was ~S." line)
       (format recv-stream "Yes I got your message, which was ~S.~%" line)
       (force-output recv-stream)
  )
)

(net:define-protocol :foo (:cyc-system-info :byte-stream)
  (:invoke-with-stream-and-close
    ((raw-stream :characters t) arg)
    (format raw-stream "~S~%" (list 'a 'b 'c arg))
    (force-output raw-stream)
    (format tv:selected-window "~&And the result was...~A" (read-line raw-stream))
  )
)


(chaos:add-contact-name-for-protocol :foo)
(tcp:add-tcp-port-for-protocol :foo 247)


;(net:invoke-service-on-host :foo "x27" 42)