;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :irc)

(defun get-day-name (day-number)
  "Given a number, such as 1, return the appropriate day name,
abbrevated, such as \"Tue\".  Index 0 is Monday."
  (case day-number
    (0 "Mon")
    (1 "Tue")
    (2 "Wed")
    (3 "Thu")
    (4 "Fri")
    (5 "Sat")
    (6 "Sun")
    (otherwise
     (error "Unknown day ~A." day-number))))

(defun get-month-name (month-number)
  "Index 1 is January."
  (case month-number
    (1 "Jan")
    (2 "Feb")
    (3 "Mar")
    (4 "Apr")
    (5 "May")
    (6 "Jun")
    (7 "Jul")
    (8 "Aug")
    (9 "Sep")
    (10 "Oct")
    (11 "Nov")
    (12 "Dec")
    (otherwise
     (error "Unknown month ~A." month-number))))

(defun make-time-message (second minute hour date month year day)
  "Returns a string composed of the input parameters so that it
represents a time message as by the IRC protocol."
  (format nil "~A ~A ~2D ~2,'0D:~2,'0D:~2,'0D ~D"
          (get-day-name day)
          (get-month-name month)
          date
          hour
          minute
          second
          year))

(defun make-irc-message (command &key (arguments nil)
                                 (trailing-argument nil))
  "Return a valid IRC message, as a string, composed of the input
parameters."
  (let ((*print-circle* nil))
    (format nil "~A~{ ~A~}~A~A~A~A" command arguments
	    (if trailing-argument
		" :"
              "")
	    (or trailing-argument "")
	    #\Return
	    #\Linefeed)))

(defun make-ctcp-message (string)
  "Return a valid IRC CTCP message, as a string, composed by
`string'."
  (format nil "~A~A~A" +soh+ string +soh+))

(defun tokenize-string (string &key
                               (delimiters '(#\Space #\Return #\Linefeed #\Newline)))
  "Split string into a list, splitting on `delimiters' and removing any
empty subsequences."
  (split-sequence:split-sequence-if #'(lambda (character)
                                        (member character delimiters))
                                    string :remove-empty-subseqs t))

(defun list-of-strings-to-integers (list)
  "Take a list of strings and return a new list of integers (from
parse-integer) on each of the string elements."
  (let ((new-list nil))
    (dolist (element (reverse list))
      (push (parse-integer element) new-list))
    new-list))

(defun host-byte-order (string)
  "Convert a string, such as 192.168.1.1, to host-byte-order, such as
3232235777."
  (let ((list (list-of-strings-to-integers (split-sequence:split-sequence #\. string))))
    (+ (* (first list) 256 256 256) (* (second list) 256 256)
       (* (third list) 256) (fourth list))))

(defun hbo-to-dotted-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (format nil "~A.~A.~A.~A" first second third fourth)))

(defun hbo-to-vector-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (vector first second third fourth)))

(defun connect-to-server-socket (host port)
  #+sbcl
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                          :type :stream
                          :protocol :tcp)))
    (sb-bsd-sockets:socket-connect s (car (sb-bsd-sockets:host-ent-addresses
                                           (sb-bsd-sockets:get-host-by-name host))) port)
    s)
  )

(defun socket-stream (socket)
  #+sbcl
  (sb-bsd-sockets:socket-make-stream socket
                                     :element-type 'character
                                     :input t
                                     :output t
                                     :buffering :none)
  )

(defun socket-connect (server port)
  "Create a socket connected to `server':`port' and return stream for it."
  #+lispworks (comm:open-tcp-stream server port :errorp t)
  #+cmu       (sys:make-fd-stream (ext:connect-to-inet-socket server port)
                                  :input t
                                  :output t
                                  :element-type 'character)
  #+allegro (socket:make-socket :remote-host server :remote-port port)
  #+sbcl (socket-stream (connect-to-server-socket server port))
  #+openmcl (ccl:make-socket :remote-host server :remote-port port)
  #+armedbear (ext:get-socket-stream (ext:make-socket server port))
  #-(or lispworks cmu allegro sbcl openmcl armedbear)
  (warn "socket-connect not supported for this implementation.")
  )


(defun cut-between (string start-char end-chars &key (start 0) (cut-extra t))
  "If `start-char' is not nil, cut string between `start-char' and any
of the `end-chars', from `start'.  If `start-char' is nil, cut from
`start' until any of the `end-chars'.

If `cut-extra' is t, we will cut from start + 1 instead of just
`start'.

When there is no string matching the input parameters `start' and nil
will be returned, otherwise `end-position' and the string are
returned."
  (let ((end-position (position-if #'(lambda (char)
                                       (member char end-chars))
                                   string :start (1+ start)))
        (cut-from (if cut-extra
                      (1+ start)
                      start)))
    (if (and end-position start-char)
        (if (eql (char string start) start-char)
            (values end-position
                    (subseq string cut-from end-position))
            (values start nil))
        (if end-position
            (values end-position
                    (subseq string cut-from end-position))
            (values start nil)))))

(defun cut-before (string substring end-chars &key (start 0) (cut-extra t))
  "Cut `string' before `substring' or any of the `end-chars', from `start'.

If `cut-extra' is t, we will cut from start + 1 instead of just
`start'.

When there is no string matching the input parameters `start' and nil
will be returned, otherwise `end-position' and the string are
returned."
  (let ((end-position (search substring string :start2 start)))
    (if end-position
        (values (+ end-position (1- (length substring)))
                (subseq string (if (and cut-extra
                                        (< start end-position))
                                   (1+ start) start) end-position))
      (let ((end-position (position-if #'(lambda (x)
                                           (member x end-chars))
                                       string :start (1+ start)))
            (cut-from (if cut-extra (1+ start) start)))
        (if end-position
            (values end-position
                    (subseq string cut-from end-position))
          (values start nil))))))

(defun parse-isupport-prefix-argument (prefix)
  (declare (type string prefix))
  (let ((closing-paren-pos (position #\) prefix)))
    (when (and (eq (elt prefix 0) #\( )
               closing-paren-pos)
      (let ((prefixes (subseq prefix (1+ closing-paren-pos)))
            (modes (subseq prefix 1 closing-paren-pos)))
        (when (= (length prefixes)
                 (length modes))
          (values prefixes modes))))))

(defun nick-prefixes-from-isupport (isupport-arguments)
  "Returns an assoc list associating prefix characters with mode characters."
  (multiple-value-bind
      (prefixes modes)
      (parse-isupport-prefix-argument (second (assoc "PREFIX"
                                                     isupport-arguments
                                                     :test #'string=)))
    (let ((rv))
      (dotimes (i (length modes)
                  rv)
        (setf (getf rv (char prefixes i))
              (char modes i))))))

(defun chanmode-descs-from-isupport (isupport-arguments
                                     &optional
                                     (mode-symbols
                                      *default-char-to-channel-modes-map*))
  "Parses a string describing channel modes conforming to
http://www.irc.org/tech_docs/draft-brocklesby-irc-isupport-03.txt
paragraph 3.3.

It returns a list of mode-description records."
  (let* ((mode-desc-recs)
         (pref (second (assoc "PREFIX" isupport-arguments :test #'string=)))
         (chanmodes (second (assoc "CHANMODES" isupport-arguments
                                   :test #'string=)))
         (modes-list
          (cons (second (multiple-value-list
                         (parse-isupport-prefix-argument pref)))
                (split-sequence:split-sequence #\, chanmodes)))
         (mode-descs '(;; B type mode from PREFIX with nick argument
                       (t t t list-value-mode)
                       ;; A type mode
                       (:optional-for-server
                        :optional-for-server nil list-value-mode)
                       ;; B type mode from CHANMODES
                       (t   t   nil single-value-mode)
                       ;; C type mode from CHANMODES
                       (t   nil nil single-value-mode)
                       ;; D type mode from CHANMODES
                       (nil nil nil boolean-value-mode))))
    (do ((mode (pop modes-list) (pop modes-list))
         (mode-desc (pop mode-descs) (pop mode-descs)))
        ((null mode-desc) mode-desc-recs)
      (when (< 0 (length mode))
        (let ((mode-struct
               (make-mode-description :param-on-set-p (first mode-desc)
                                      :param-on-unset-p (second mode-desc)
                                      :nick-param-p (third mode-desc)
                                      :class (fourth mode-desc))))
          (dotimes (j (length mode))
            (let ((mode-rec (copy-structure mode-struct))
                  (mode-char (elt mode j)))
              (setf (mode-desc-char mode-rec) mode-char
                    (mode-desc-symbol mode-rec) (cdr (assoc mode-char
                                                            mode-symbols)))
              (push mode-rec mode-desc-recs))))))))

(defmacro do-property-list ((prop val list) &body body)
  (let ((lsym (gensym)))
    `(let ((,lsym ,list))
       (do* ((,prop (pop ,lsym) (pop ,lsym))
             (,val (pop ,lsym) (pop ,lsym)))
           ((and (null ,lsym)
                 (null ,prop)
                 (null ,val)))
         ,@body))))

(defgeneric irc-string-downcase (map-name string &key start end))

(defmethod irc-string-downcase (map-name
                                string &key (start 0) end)
  (declare (ignore map-name))
  (let* ((new-string (substitute #\[ #\{ string :start start :end end))
         (new-string (substitute #\] #\} new-string :start start :end end))
         (new-string (substitute #\\ #\| new-string :start start :end end))
         (new-string (substitute #\~ #\^ new-string :start start :end end)))
    (string-downcase new-string :start start :end end)))

(defmethod irc-string-downcase ((map-name (eql :ascii))
                                string &key (start 0) end)
  (declare (ignore map-name))
  (string-downcase string :start start :end end))

(defun parse-isupport-multivalue-argument (argument)
  (declare (type string argument))
  (mapcar #'(lambda (x)
              (split-sequence:split-sequence #\: x))
          (split-sequence:split-sequence #\, argument)))

(defun parse-mode-arguments (connection target arguments &key server-p)
  "Create a list of mode changes with their arguments for `target'
   from `mode-string' and `arguments'.

   Throw nil to the UNKNOWN-MODE symbol if any of the mode chars are unknown."
  (catch 'illegal-mode-spec
    (if (and (= 1 (length arguments))
             (null (position (char (first arguments) 0) "+-")))
        ;; type 1 mode specification; only allowed on servers
        (when server-p
          (let ((ops)
                (arg (car arguments)))
            (dotimes (i (length arg) (reverse ops))
              (push (char arg i) ops))))
      ;; type 2 mode specification; clients and servers
      (let ((ops))
        (do ((changes (pop arguments) (pop arguments)))
            ((null changes) (values ops nil))
          (let* ((this-op (char changes 0))
                 (modes (subseq changes 1))
                 (param-req (if (char= this-op #\+)
                                #'mode-desc-param-on-set-p
                              #'mode-desc-param-on-unset-p)))
            (unless (position this-op "+-")
              (throw 'illegal-mode-spec nil))
            (dotimes (i (length modes))
              (let* ((mode-rec
                      (mode-description connection target
                                        (mode-name-from-char connection target
                                                             (char modes i))))
                     (param-p (funcall param-req mode-rec)))
                (when (and param-p
                           (= 0 (length arguments)))
                  (throw 'illegal-mode-spec nil))
                (push (list this-op
                            (mode-desc-symbol mode-rec)
                            (when param-p
                              (if (mode-desc-nick-param-p mode-rec)
                                  (find-user connection (pop arguments))
                                (pop arguments)))) ops)))))))))

