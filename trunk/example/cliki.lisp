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

(defvar *aliases* nil)

(defun forget (term-or-alias)
  (setf *small-definitions* (remove term-or-alias *small-definitions* :test #'string-equal :key #'car))
  (setf *aliases* (remove term-or-alias *aliases* :test #'string-equal :key #'car))
  (write-small-definitions))

(defun fix-aliases ()
  (setf *small-definitions*
        (loop for defn in *small-definitions*
              if (stringp (cdr defn))
              collect defn
              else do (push (cons (first defn) (second defn))
                            *aliases*))))

(defun read-small-definitions ()
  (setf *small-definitions* nil)
  (setf *aliases* nil)
  (with-open-file (sd-file "sd.lisp-expr" :direction :input :if-does-not-exist nil)
    (when sd-file
      (loop for defn = (read sd-file nil)
            if defn do (ecase (car defn)
                         (:sd (push (cdr defn) *small-definitions*))
                         (:alias (push (cdr defn) *aliases*)))
            else return *small-definitions*))))

(defun write-small-definitions ()
  (with-open-file (sd-file "sd.lisp-expr" :direction :output :if-exists :supersede)
    (mapc #'(lambda (db)
              (mapc #'(lambda (defn)
                        (prin1 (cons (car db) defn) sd-file)
                        (format sd-file "~%")) (reverse (cdr db))))
          (list (cons :sd *small-definitions*)
                (cons :alias *aliases*)))))

(defun write-top-definition (&key (of *small-definitions*) (type :sd))
  (with-open-file (sd-file "sd.lisp-expr" :direction :output :if-exists :append)
    (prin1 (cons type (car of)) sd-file)
    (format sd-file "~%")))

(defun add-small-definition (term defn)
  (push (cons term defn) *small-definitions*)
  (write-top-definition))

(defun add-alias (term defn)
  (push (cons term defn) *aliases*)
  (write-top-definition :of *aliases* :type :alias))

(defvar *lookup-depth* 0)

(defvar *followed-aliases* nil)

(defun alias-string-equal (orig candidate)
  (unless (member candidate *followed-aliases* :test #'string-equal)
    (string-equal orig candidate)))

(defun small-definition-lookup (text)
  (cdr (assoc text *small-definitions* :test #'string-equal)))

(defun alias-lookup (text)
  (let ((alias (or (cdr (assoc text *aliases* :test #'alias-string-equal))
                   (car (rassoc text *aliases* :test #'alias-string-equal)))))
    (if alias
        (let ((*lookup-depth* (1+ *lookup-depth*))
              (*followed-aliases* (cons alias *followed-aliases*)))
          (if (> *lookup-depth* 5)
              "Too many recursive lookups."
              (cliki-lookup alias))))))

(defclass memo ()
  ((from :accessor memo-from :initarg :from)
   (to :accessor memo-to :initarg :to)
   (contents :accessor memo-contents :initarg :contents)))

(defun without-non-alphanumeric (string)
  (with-output-to-string (s)
    (loop for char across string
          if (alphanumericp char)
          do (princ char s))))

(defvar *pending-memos* nil)

(defun memo-alias-test (orig candidate)
  (or (string-equal orig (car candidate))
      (string-equal orig (cdr candidate))
      (string-equal orig (without-non-alphanumeric (car candidate)))
      (string-equal orig (without-non-alphanumeric (cdr candidate)))))

(defun take-care-of-memos (channel user &key (original-user user) (no-alias nil))
  (let ((found (find (without-non-alphanumeric user) *pending-memos* :test #'string-equal :key #'memo-to :from-end t)))
    (if found
        (progn
          (setf *pending-memos* (remove found *pending-memos*))
          (privmsg *cliki-connection* channel (format nil "~A, memo from ~A: ~A" original-user (memo-from found) (memo-contents found)))
          (take-care-of-memos channel user :original-user original-user))
        (if (not no-alias)
            (let ((alias (find (without-non-alphanumeric user)
                               *aliases*
                               :test #'memo-alias-test)))
              (if alias
                  (take-care-of-memos channel (cdr alias) :original-user original-user :no-alias t)))))))
  
(defun add-memo (from to contents)
  (push (make-instance 'memo :from from
                       :to (without-non-alphanumeric to)
                       :contents contents)
        *pending-memos*))

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

#-(or sbcl ccl)
(defmacro host-with-timeout (timeout &body body)
  (declare (ignore timeout))
  `(progn ,@body))

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
			    (setf first-line (regex-replace-all "^\\s*(.+\\S)\\s*$" first-line "\\1"))
			    (when (scan "^([^.]|\\.\\S)+[.?!]$" first-line)
				(setf first-line (concatenate 'string first-line " " cliki-url))
				(return-from cliki-return first-line))))
			 (format nil "No definition was found in the first 5 lines of ~A" cliki-url)))
		(if stream (close stream)))))
	(condition (c &rest whatever) (return-from cliki-return (regex-replace-all "\\n" (format nil "An error was encountered in lookup: ~A." c) " ")))))))

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

(defun make-cliki-attention-prefix (nick)
  (format nil "^~A[,:]\\s+" nick))

(defvar *cliki-attention-prefix* "")

(defparameter *cliki-bot-help* "The minion bot supplies small definitions and performs lookups on CLiki. To use it, try ``minion: term?''. To add a term for IRC, try saying ``minion: add \"term\" as: definition'' or ``minion: alias \"term\" as: term''; otherwise, edit the corresponding CLiki page.")

(defun cliki-lookup (term-with-question &key sender channel)
  (let ((first-pass (regex-replace-all "^(\\s*)([^?]+)(\\?*)$" term-with-question "\\2")))
    (setf first-pass (regex-replace-all "\\s\\s+" first-pass ""))
    (setf first-pass (regex-replace-all "\\s*$" first-pass ""))
    (let ((scanned (or (nth-value 1 (scan-to-strings "^add\\s+\"([^\"]+)\"\\s+as:*\\s+(.+)$" first-pass))
                        (nth-value 1 (scan-to-strings "^add\\s+(.+)\\s+as:\\s+(.+)$" first-pass)))))
      (if scanned
          (let ((term (elt scanned 0))
                (defn (elt scanned 1)))
            (add-small-definition term defn)
            "OK, done.")
	(let ((scanned (or
                        (nth-value 1 (scan-to-strings "^alias\\s+\"([^\"]+)\"\\s+as:*\\s+(.+)$" first-pass))
                        (nth-value 1 (scan-to-strings "^alias\\s+(.+)\\s+as:*\\s+(.+)$" first-pass))
                        (nth-value 1 (scan-to-strings "^(.+)\\s+is\\s+another\\s+(name|word)\\s+for:*\\s+([^.]+)\\.*$" first-pass)))))
          (if scanned
              (let ((term (elt scanned 0))
                    (defn (elt scanned (1- (length scanned)))))
                (add-alias term defn)
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
                 (let ((strings (nth-value 1 (scan-to-strings "^(?i)memo\\s+(for|to)\\s+(\\S+)\\s+:*\\s*(.+)$" first-pass))))
                   (when (and sender strings)
                     (add-memo
                      sender
                      (if (member (elt strings 1) '("self" "myself" "me") :test #'string-equal)
                          sender
                          (elt strings 1))
                      (elt strings 2))
                     (format nil "Remembered. I'll tell ~A when he/she/it next speaks." (elt strings 1))))
                 (let ((to-forget (nth-value 1 (scan-to-strings "^forget\\s+([^.]+)\\.*$" first-pass))))
                   (when to-forget
                     (forget (elt to-forget 0))
                     (format nil "What's ~A? Never heard of it." (elt to-forget 0))))
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
                 (aif (or (small-definition-lookup first-pass)
                          (cliki-first-sentence first-pass)
                          (alias-lookup first-pass)) (concatenate 'string first-pass ": " it))
                 (if (or
                      (scan "(!|\\.|\\s.+\\?|\\)|\\()\\s*$" term-with-question)
                      (scan "^\\s*\\S+\\s+\\S+.*$" term-with-question))
                     ;;(generate-text (+ 20 (random 6)))
                     (ignore-errors (eliza::eliza first-pass))
                     )
                 (format nil "Sorry, I couldn't find anything in the database for ``~A''.~A" first-pass (if (scan " " first-pass) " Maybe you meant to end with punctuation?" ""))
                 ))))))))
    
(defun valid-cliki-message (message)
  (scan *cliki-attention-prefix* (trailing-argument message)))

(defvar *respond-to-general-hellos* nil)

(defun anybody-here (string)
  (if *respond-to-general-hellos*
      (or (scan "(?i)(anybody|aynbody|any body|anyone|aynone|any one|ne1|any1|n e 1|ne 1) (here|awake|there|home|know).*\\?*" string)
	  (scan "^(?i)\\s*(hello|hi|yo)\\s*(channel|room|people|ppl|all|peeps|)\\s*$" string))))

(defun msg-hook (message)
  (let ((respond-to (if (string-equal (first (arguments message)) *cliki-nickname*) (source message) (first (arguments message)))))
    (take-care-of-memos respond-to (source message))
    (if (valid-cliki-message message)
        (let ((response (cliki-lookup (regex-replace *cliki-attention-prefix* (trailing-argument message) "") :sender (source message) :channel (first (irc:arguments message)))))
          (and response (privmsg *cliki-connection* respond-to response)))
      (if (string-equal (first (arguments message)) *cliki-nickname*)
	  (privmsg *cliki-connection* respond-to (cliki-lookup (trailing-argument message) :sender (source message)))
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
  (setf *cliki-attention-prefix* (make-cliki-attention-prefix nick))
  (mapcar #'(lambda (channel) (join *cliki-connection* channel)) channels)
  (add-hook *cliki-connection* 'irc::irc-privmsg-message 'msg-hook)
  (add-hook *cliki-connection* 'irc::irc-notice-message 'notice-hook)
  (start-background-message-handler *cliki-connection*))

(defun shuffle-hooks ()
  (irc::remove-hooks *cliki-connection* 'irc::irc-privmsg-message)
  (add-hook *cliki-connection* 'irc::irc-privmsg-message 'msg-hook))
