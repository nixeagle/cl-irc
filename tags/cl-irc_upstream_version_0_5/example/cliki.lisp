;;;; $Id$
;;;; $Source$

;;;; cliki.lisp - CLiki as an infobot; only works on SBCL.

;;; To use it, load the cl-irc and cl-ppcre systems, load
;;; cliki.lisp, and invoke (cliki::start-cliki-bot "desirednickname"
;;; "desiredserver" "#channel1" "#channel2" "#channel3" ...)

(defpackage :cliki (:use :common-lisp :irc :sb-bsd-sockets :cl-ppcre))
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

(defun url-connection (url)
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp))
	(host (url-host url))
	(port (url-port url)))
    (declare (ignore port))
    (socket-connect
     s (car (host-ent-addresses (get-host-by-name (url-host url))))
     (url-port url))
    (let ((stream (socket-make-stream s :input t :output t :buffering :full)))
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
       stream))))

(defun encode-for-url (str)
  (setf str (regex-replace-all " " str "%20"))
  (setf str (regex-replace-all "," str "%2C"))
  (setf str (regex-replace-all "`" str "%60"))
  ;(format t "hi ~A~%" str)
  str)

(defun cliki-first-sentence (term)
  (let* ((cliki-url (format nil "http://www.cliki.net/~A"
		     (encode-for-url term)))
	 (url (concatenate 'string cliki-url "?source")))
    (block cliki-return
      (handler-case
	  (sb-ext:with-timeout 5
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
		       (format nil "The term ~A was not found in CLiki." term)
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

(defmacro aif (test conseq &optional (else nil))
  `(let ((it ,test))
     (if it ,conseq
       (symbol-macrolet ((it ,test))
         ,else))))

(defparameter *cliki-attention-prefix* "minion: ")

(defparameter *cliki-bot-help* "The minion bot supplies small definitions and performs lookups on CLiki. To use it, try ``minion: term?''. To add a term for IRC, try saying ``minion: add \"term\" as: definition'' or ``minion: alias \"term\" as: term''; otherwise, edit the corresponding CLiki page.")

(defun cliki-lookup (term-with-question)
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
	    (or
	     (if (string-equal first-pass "help") *cliki-bot-help*)
	     (if (scan "^(?i)do my bidding!*$" first-pass) "Yes, my master.")
	     (concatenate 'string first-pass ": "
			  (or (let ((term (cdr (assoc first-pass *small-definitions* :test #'string-equal))))
				(if term (if (stringp term) term (cliki-lookup (car term)))))
			      (cliki-first-sentence first-pass))))))))

(defun valid-cliki-message (message)
  (eql (search *cliki-attention-prefix* (trailing-argument message) :test #'char-equal) 0))

(defun msg-hook (message)
  (if (string-equal (first (arguments message)) *cliki-nickname*)
      (if (valid-cliki-message message)
          (privmsg *cliki-connection* (source message) (cliki-lookup (subseq (trailing-argument message) (length *cliki-attention-prefix*))))
        (privmsg *cliki-connection* (source message) (cliki-lookup (trailing-argument message))))
    (if (valid-cliki-message message)
        (privmsg *cliki-connection* (first (arguments message)) (cliki-lookup (subseq (trailing-argument message) (length *cliki-attention-prefix*)))))))

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
  #+sbcl (start-background-message-handler *cliki-connection*)
  #-sbcl (read-message-loop *cliki-connection*))

(defun shuffle-hooks ()
  (irc::remove-hooks *cliki-connection* 'irc::irc-privmsg-message)
  (add-hook *cliki-connection* 'irc::irc-privmsg-message 'msg-hook))
