;;;; $Id$
;;;; $Source$

;;;; cliki.lisp - CLiki as an infobot; only works on SBCL.

;;; To use it, load the net-nittin-irc and cl-ppcre systems, load
;;; cliki.lisp, and invoke (cliki::start-cliki-bot "desirednickname"
;;; "desiredserver" "#channel1" "#channel2" "#channel3" ...)

(defpackage :cliki (:use :common-lisp :irc :cl-ppcre)
  (:export :start-cliki-bot :*cliki-nickserv-password*
	   :*respond-to-general-hellos* :shut-up :un-shut-up))
(in-package :cliki)


(defvar *small-definitions* nil)

(defun read-small-definitions ()
  (setf *small-definitions* nil)
  (with-open-file (sd-file "sd.lisp-expr" :direction :input :if-does-not-exist nil)
    (when sd-file
      (block nil
	(loop (let ((defn (read sd-file nil)))
		(if defn (push defn *small-definitions*)
		    (return (setf *small-definitions* (nreverse *small-definitions*))))))))))

(defun write-small-definitions ()
  (with-open-file (sd-file "sd.lisp-expr" :direction :output :if-exists :supersede)
    (mapc #'(lambda (defn)
	      (prin1 defn sd-file)
	      (format sd-file "~%")) *small-definitions*)))

(defun write-top-definition ()
  (with-open-file (sd-file "sd.lisp-expr" :direction :output :if-exists :append)
    (prin1 (car *small-definitions*) sd-file)
    (format sd-file "~%")))

(defun add-small-definition (term defn)
  (push (cons term defn) *small-definitions*)
  (write-small-definitions))

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
  (let ((port-start (position #\: url :start 7)))
    (if port-start (parse-integer url :start (1+ port-start) :junk-allowed t) 80)))

(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
	 (host-end (min (or (position #\/ url :start 7) (length url))
			(or port-start (length url)))))
    (subseq url 7 host-end)))

#+(or ccl allegro)
(defun socket-connect (host port)
  (#+ccl ccl:make-socket
         #+allegro socket:make-socket
         :connect :active
         :remote-host host
         :remote-port port))

#+sbcl
(defun socket-connect (host port)
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                          :type :stream
                          :protocol :tcp)))
    (sb-bsd-sockets:socket-connect s (car (sb-bsd-sockets:host-ent-addresses
                                           (sb-bsd-sockets:get-host-by-name host))) port)
    (sb-bsd-sockets:socket-make-stream s
                                       :element-type 'character
                                       :input t
                                       :output t
                                       :buffering :none)))

(defun url-connection (url)
  (let* ((host (url-host url))
         (port (url-port url))
         (stream (socket-connect host port)))
    ;; we are exceedingly unportable about proper line-endings here.
    ;; Anyone wishing to run this under non-SBCL should take especial care
    (format stream "GET ~A HTTP/1.0~%Host: ~A~%User-Agent: CLiki Bot~%~%" url host)
    (force-output stream)
    (list
     (let* ((l (read-line stream))
            (space (position #\Space l)))
       (parse-integer l :start (1+ space) :junk-allowed t))
     (loop for line = (read-line stream nil nil)
           until (or (null line) (eql (elt line 0) (code-char 13)))
           collect
           (let ((colon (position #\: line)))
             (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
                   (string-trim (list #\Space (code-char 13))
                                (subseq line (1+ colon))))))
     stream)))

(defun encode-for-url (str)
  (setf str (regex-replace-all " " str "%20"))
  (setf str (regex-replace-all "," str "%2C"))
  (setf str (regex-replace-all "`" str "%60"))
  ;(format t "hi ~A~%" str)
  str)

#+sbcl
(defmacro host-with-timeout (timeout &body body)
  `(sb-ext:with-timeout ,timeout ,@body))

#+ccl
(defmacro host-with-timeout (timeout &body body)
  `(let ((interrupt-thread nil))
    (setf interrupt-thread
     (ccl:process-run-function 'timeout
      (let ((process ccl:*current-process*))
        (lambda ()
          (sleep ,timeout)
          (ccl:process-interrupt process
                                 (lambda ()
                                   (signal 'openmcl-timeout)))))))
    (unwind-protect
         (progn ,@body)
      (if interrupt-thread
          (ccl:process-kill interrupt-thread)))))

(defun cliki-first-sentence (term)
  (let* ((cliki-url (format nil "http://www.cliki.net/~A"
		     (encode-for-url term)))
	 (url (concatenate 'string cliki-url "?source")))
    (block cliki-return
      (handler-case
	  (host-with-timeout 5
	    (destructuring-bind (response headers stream)
		(block got
		  (loop
		     (destructuring-bind (response headers stream) (url-connection url)
		       (unless (member response '(301 302))	       
			 (return-from got (list response headers stream)))
		       (close stream)
		       (setf url (cdr (assoc :location headers))))))
	      (unwind-protect
		   (if (not (eql response 200))
                       nil
		       ;;(format nil "The term ~A was not found in CLiki." term)
		       (let ((first-line ""))
			 (loop for i from 1 to 5 do ;; scan the first 5 lines
			  (progn
			    (multiple-value-bind (next-line missing-newline-p)
				(read-line stream nil)
			      (if next-line
				  (setf first-line (concatenate 'string first-line next-line (string #\newline)))
				  (return-from cliki-return (format nil "The end of the page was reached before a definition was found in ~A" cliki-url))))
			    (setf first-line (regex-replace-all "\\r" first-line " "))
			    (setf first-line (regex-replace-all "\\n" first-line " "))
			    (setf first-line (regex-replace-all "_\\(([^)]*)\\)" first-line "\\1"))
			    (setf first-line (regex-replace-all "\\*\\(([^)]*)\\)" first-line "\\1"))
			    (setf first-line (regex-replace-all "<[^>]+>" first-line ""))
			    (setf first-line (regex-replace-all "^(([^.]|\\.\\S)+)\\.\\s+.*$" first-line "\\1."))
			    (setf first-line (regex-replace-all "(\\s)\\s+" first-line "\\1"))
			    (setf first-line (regex-replace-all "^\\s(.+)$" first-line "\\1"))
			    (when (scan "^([^.]|\\.\\S)+\\.$" first-line)
				(setf first-line (concatenate 'string first-line " " cliki-url))
				(return-from cliki-return first-line))))
			 (format nil "No definition was found in the first 5 lines of ~A" cliki-url)))
		(if stream (close stream)))))
	(condition (c &rest whatever) (return-from cliki-return (format nil "An error was encountered in lookup.")))))))

(defvar *cliki-connection*)
(defvar *cliki-nickname*)

(defun shut-up ()
  (setf (irc:client-stream *cliki-connection*) (make-broadcast-stream)))

(defun un-shut-up ()
  (setf (irc:client-stream *cliki-connection*) *trace-output*))


(defmacro aif (test conseq &optional (else nil))
  `(let ((it ,test))
     (if it ,conseq
       (symbol-macrolet ((it ,test))
         ,else))))

(defparameter *cliki-attention-prefix* "^minion[,:]\\s+")

(defparameter *cliki-bot-help* "The minion bot supplies small definitions and performs lookups on CLiki. To use it, try ``minion: term?''. To add a term for IRC, try saying ``minion: add \"term\" as: definition'' or ``minion: alias \"term\" as: term''; otherwise, edit the corresponding CLiki page.")

(defun cliki-lookup (term-with-question &key sender channel)
  (let ((first-pass (regex-replace-all "^(\\s*)([^?]+)(\\?*)$" term-with-question "\\2")))
    (setf first-pass (regex-replace-all "\\s\\s+" first-pass ""))
    (setf first-pass (regex-replace-all "\\s*$" first-pass ""))
    (if (scan "^add \"([^\"]+)\" as: (.+)$" first-pass)
	(let ((term (regex-replace "^add \"([^\"]+)\" .*$" first-pass "\\1"))
	      (defn (regex-replace "^add \"[^\"]+\" as: (.+)$" first-pass "\\1")))
	  (add-small-definition term defn)
	  "OK, done.")
	(if (scan "^alias \"([^\"]+)\" as: (.+)$" first-pass)
	    (let ((term (regex-replace "^alias \"([^\"]+)\" .*$" first-pass "\\1"))
		  (defn (regex-replace "^alias \"[^\"]+\" as: (.+)$" first-pass "\\1")))
	      (add-small-definition term (list defn))
	      "OK, done.")
	  (progn
	    (setf first-pass (regex-replace-all "(:|/|\\\\|\\#)" first-pass ""))
            (when (and (scan "^(?i)lisppaste(\\s|!|\\?|\\.|$)*" first-pass)
                        (find-package :lisppaste)
                        channel
                        (> (length channel) 0)
                        (char= (elt channel 0) #\#)
                        (funcall (intern "SAY-HELP" :lisppaste)
                                 channel))
               (return-from cliki-lookup nil))
	    (or
	     (if (string-equal first-pass "help") *cliki-bot-help*)
             (if (scan "^(?i)hello(\\s|$)*" first-pass) "what's up?")
	     (if (scan "^(?i)hi(\\s|$)*" first-pass) "what's up?")
	     (if (scan "^(?i)yo(\\s|$)*" first-pass) "what's up?")
	     (if (scan "^(?i)thank(s| you)(\\s|!|\\?|\\.|$)*" first-pass)
		 (if sender
		     (format nil "~A: you failed the inverse turing test!" sender)
		   "you failed the inverse turing test!"))
             (if (scan "^(?i)version(\\s|!|\\?|\\.|$)*" first-pass)
                 (format nil "This is the minion bot, running on a ~A (~A) and running under ~A ~A." (machine-type) (machine-version) (lisp-implementation-type) (lisp-implementation-version)))
	     (if (scan "^(?i)(?i)do my bidding!*$" first-pass) "Yes, my master.")
             (aif (or (let ((term (cdr (assoc first-pass *small-definitions* :test #'string-equal))))
                        (if term (if (stringp term) term (cliki-lookup (car term)))))
                      (cliki-first-sentence first-pass)) (concatenate 'string first-pass ": " it))
             (if (scan "(!|\\.|\\s.+\\?|\\)|\\()\\s*$" term-with-question)
		 ;;(generate-text (+ 20 (random 6)))
		 (ignore-errors (eliza::eliza first-pass))
	       )
	     (format nil "Sorry, I couldn't find anything in the database for ``~A''.~A" first-pass (if (scan " " first-pass) " Maybe you meant to end with punctuation?" ""))
                  ))))))

(defun valid-cliki-message (message)
  (scan *cliki-attention-prefix* (trailing-argument message)))

(defvar *respond-to-general-hellos* nil)

(defun anybody-here (string)
  (if *respond-to-general-hellos*
      (or (scan "(?i)(anybody|aynbody|any body|anyone|aynone|any one|ne1|any1|n e 1|ne 1) (here|awake|there|home|know).*\\?*" string)
	  (scan "^(?i)\\s*(hello|hi|yo)\\s*(channel|room|people|ppl|all|peeps|)\\s*$" string))))

(defun msg-hook (message)
  (let ((respond-to (if (string-equal (first (arguments message)) *cliki-nickname*) (source message) (first (arguments message)))))
    (if (valid-cliki-message message)
        (let ((response (cliki-lookup (regex-replace *cliki-attention-prefix* (trailing-argument message) "") :sender (source message) :channel (first (irc:arguments message)))))
          (and response (privmsg *cliki-connection* respond-to response)))
      (if (string-equal (first (arguments message)) *cliki-nickname*)
	  (privmsg *cliki-connection* respond-to (cliki-lookup (trailing-argument message)))
	(if (anybody-here (trailing-argument message))
	    (privmsg *cliki-connection* (first (arguments message)) (format nil "~A: hello." (source message))))))))

(defvar *cliki-nickserv-password* "")

(defun notice-hook (message)
  (if (and (string-equal (source message) "NickServ")
	   (scan "owned by someone else" (trailing-argument message)))
      (privmsg *cliki-connection* (source message) (format nil "IDENTIFY ~A" *cliki-nickserv-password*))))

(defun start-cliki-bot (nick server &rest channels)
  (read-small-definitions)
  (setf *cliki-nickname* nick)
  (setf *cliki-connection* (connect :nickname *cliki-nickname* :server server))
  (mapcar #'(lambda (channel) (join *cliki-connection* channel)) channels)
  (add-hook *cliki-connection* 'irc::irc-privmsg-message 'msg-hook)
  (add-hook *cliki-connection* 'irc::irc-notice-message 'notice-hook)
  (start-background-message-handler *cliki-connection*))

(defun shuffle-hooks ()
  (irc::remove-hooks *cliki-connection* 'irc::irc-privmsg-message)
  (add-hook *cliki-connection* 'irc::irc-privmsg-message 'msg-hook))
