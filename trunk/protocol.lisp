;;;; $Id$
;;;; $Source$

;;;; See LICENSE for licensing information.

(in-package :irc)

;;
;; Condition
;;

(define-condition no-such-reply ()
  ((reply-number
    :reader reply-number
    :initarg :reply-number))
  (:report (lambda (condition stream)
             (format stream "No such reply ~A." (reply-number condition)))))

;;
;; Connection
;;


(defclass connection ()
  ((user
    :initarg :user
    :accessor user)
   (server-name
    :initarg :server-name
    :accessor server-name
    :initform "Unknown server")
   (server-stream
    :initarg :server-stream
    :accessor server-stream
    :documentation "Stream used to talk to the IRC server.")
   (server-capabilities
    :initform *default-isupport-values*
    :accessor server-capabilities
    :documentation "Assoc array for rpl_isupport message;
see http://www.irc.org/tech_docs/draft-brocklesby-irc-isupport-03.txt")
   (client-stream
    :initarg :client-stream
    :accessor client-stream
    :initform t
    :documentation "Messages coming back from the server is sent to
this stream.")
   (channels
    :initarg :channels
    :accessor channels
    :initform (make-hash-table :test #'equal))
   (hooks
    :initarg :hooks
    :accessor hooks
    :initform (make-hash-table :test #'equal))
   (users
    :initarg :users
    :accessor users
    :initform (make-hash-table :test #'equal))))

(defmethod print-object ((object connection) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (princ (server-name object) stream)))

(defgeneric add-default-hooks (connection))
(defgeneric client-raw-log (connection message))
(defgeneric connectedp (connection))
(defgeneric read-message (connection))
(defgeneric start-process (function name))
(defgeneric start-background-message-handler (connection))
(defgeneric read-message-loop (connection))
(defgeneric read-irc-message (connection))
(defgeneric send-irc-message (connection command
                             &optional trailing-argument &rest arguments))
(defgeneric get-hooks (connection class))
(defgeneric add-hook (connection class hook))
(defgeneric remove-hook (connection class hook))
(defgeneric remove-hooks (connection class))
(defgeneric remove-all-hooks (connection))

(defgeneric case-map-name (connection))
(defgeneric re-apply-case-mapping (connection))

(defun make-connection (&key (user nil)
                             (server-name "")
                             (server-stream nil)
                             (client-stream t)
                             (hooks nil))
  (let ((connection (make-instance 'connection
                                   :user user
                                   :server-name server-name
                                   :server-stream server-stream
                                   :client-stream client-stream)))
    (dolist (hook hooks)
      (add-hook connection (car hook) (cadr hook)))
    connection))

(defmethod add-default-hooks ((connection connection))
  (dolist (message '(irc-rpl_isupport-message
                     irc-rpl_whoisuser-message
                     irc-rpl_list-message
                     irc-rpl_topic-message
                     irc-rpl_namreply-message
                     irc-ping-message
                     irc-join-message
                     irc-topic-message
                     irc-part-message
                     irc-quit-message
                     irc-kick-message
                     irc-nick-message
                     ctcp-time-message
                     ctcp-source-message
                     ctcp-finger-message
                     ctcp-version-message
                     ctcp-ping-message))
      (add-hook connection message #'default-hook)))

(defmethod client-raw-log ((connection connection) message)
  (let ((stream (client-stream connection)))
    (format stream (format nil "RAW LOG: ~A~%" message))
    (force-output stream)))

(defmethod connectedp ((connection connection))
  "Returns t if `connection' is connected to a server and is ready for
input."
  (let ((stream (server-stream connection)))
    (and (streamp stream)
         (open-stream-p stream))))

(define-condition invalidate-me (condition)
  ((stream :initarg :stream
           :reader invalidate-me-stream)
   (condition :initarg :condition
              :reader invalidate-me-condition)))

(defmethod read-message ((connection connection))
  (let ((read-more-p t))
    (handler-case
        (progn
          (when (and (connectedp connection) read-more-p)
            (let ((message (read-irc-message connection)))
              (when *debug-p*
                (format *debug-stream* "~A" (describe message)))
              (irc-message-event message)
              message))) ; needed because of the "loop while" in read-message-loop
        (stream-error (c) (setf read-more-p nil)
                    (signal 'invalidate-me :stream
                            (server-stream connection)
                            :condition c)))))

(defvar *process-count* 0)

(defmethod start-process (function name)
  #+allegro (mp:process-run-function name function)
  #+cmu (mp:make-process function :name name)
  #+lispworks (mp:process-run-function name nil function)
  #+sb-thread (sb-thread:make-thread function)
  #+openmcl (ccl:process-run-function name function)
  #+armedbear (ext:make-thread function))

(defmethod start-background-message-handler ((connection connection))
  "Read messages from the `connection', parse them and dispatch
irc-message-event on them. Returns background process ID if available."
  (flet ((do-loop () (read-message-loop connection)))
    (let ((name (format nil "irc-hander-~D" (incf *process-count*))))
      #+(or allegro cmu lispworks sb-thread openmcl armedbear)
      (start-process #'do-loop name)
      #+(and sbcl (not sb-thread))
      (sb-sys:add-fd-handler (sb-sys:fd-stream-fd
			      (server-stream connection))
			     :input (lambda (fd)
				      (declare (ignore fd))
                                      (handler-case
                                          (read-message connection)
                                        (invalidate-me (c)
                                          (sb-sys:invalidate-descriptor
                                           (sb-sys:fd-stream-fd
                                            (invalidate-me-stream c)))
                                          (format t "Socket closed: ~A~%"
                                                  (invalidate-me-condition c)))))))))

(defun stop-background-message-handler (process)
  "Stops a background message handler process returned by the start function."
    #+cmu (mp:destroy-process process)
    #+allegro (mp:process-kill process)
    #+sb-thread (sb-thread:destroy-thread process)
    #+lispworks (mp:process-kill process)
    #+openmcl (ccl:process-kill process)
    #+armedbear (ext:destroy-thread process))

(defmethod read-message-loop ((connection connection))
  (loop while (read-message connection)))

(defmethod read-irc-message ((connection connection))
  "Read and parse an IRC-message from the `connection'."
  (let ((message (create-irc-message
                  (read-line (server-stream connection) t))))
    (setf (connection message) connection)
    message))

(defmethod send-irc-message ((connection connection) command
                             &optional trailing-argument &rest arguments)
  "Turn the arguments into a valid IRC message and send it to the
server, via the `connection'."
  (let ((raw-message (make-irc-message command
                                       :arguments arguments
                                       :trailing-argument trailing-argument)))
    (write-sequence raw-message (server-stream connection))
    (force-output (server-stream connection))
    raw-message))

(defmethod get-hooks ((connection connection) (class symbol))
  "Return a list of all hooks for `class'."
  (gethash class (hooks connection)))

(defmethod add-hook ((connection connection) class hook)
  "Add `hook' to `class'."
  (setf (gethash class (hooks connection))
        (pushnew hook (gethash class (hooks connection)))))

(defmethod remove-hook ((connection connection) class hook)
  "Remove `hook' from `class'."
  (setf (gethash class (hooks connection))
        (delete hook (gethash class (hooks connection)))))

(defmethod remove-hooks ((connection connection) class)
  "Remove all hooks for `class'."
  (setf (gethash class (hooks connection)) nil))

(defmethod remove-all-hooks ((connection connection))
  (clrhash (hooks connection)))

(defmethod case-map-name ((connection connection))
  (let ((case-mapping (assoc "CASEMAPPING" (server-capabilities connection)
                             :test #'equal)))
    (intern (string-upcase (second case-mapping)) (find-package "KEYWORD"))))

(defmethod re-apply-case-mapping ((connection connection))
  (setf (normalized-nickname (user connection))
        (normalize-nickname connection (nickname (user connection))))
  (flet ((set-new-users-hash (object)
           (let ((new-users (make-hash-table :test #'equal)))
             (maphash
              #'(lambda (norm-nick user)
                  (declare (ignore norm-nick))
                  (setf (gethash
                         (setf (normalized-nickname user)
                               (normalize-nickname connection
                                                   (nickname user)))
                         new-users) user))
              (users object))
             (setf (users object) new-users))))

    (set-new-users-hash connection)
    (let ((new-channels (make-hash-table :test #'equal)))
      (maphash #'(lambda (norm-name channel)
                   (declare (ignore norm-name))
                   (setf (gethash
                          (setf (normalized-channel-name channel)
                                (normalize-channel-name connection
                                                        (name channel)))
                          new-channels) channel)
                   (set-new-users-hash channel))
               (channels connection))
      (setf (channels connection) new-channels))))


;;
;; DCC Connection
;;

(defclass dcc-connection ()
  ((user
    :initarg :user
    :accessor user
    :documentation "The user at the other end of this connection.  The
user at this end can be reached via your normal connection object.")
   (stream
    :initarg :stream
    :accessor dcc-stream)
   (output-stream
    :initarg :output-stream
    :accessor output-stream
    :initform t)
   (socket
    :initarg :socket
    :accessor socket
    :documentation "The actual socket object for the connection
between the two users.")))

(defmethod print-object ((object dcc-connection) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (if (user object)
        (format stream "with ~A@~A"
                (nickname (user object))
                (hostname (user object)))
      "")))

(defun make-dcc-connection (&key (user nil)
                                 (remote-address nil)
                                 (remote-port nil)
                                 (output-stream t))
  #+sbcl
  (let ((socket (sb-bsd-sockets:make-inet-socket :stream :tcp)))
    (sb-bsd-sockets:socket-connect socket remote-address remote-port)
    (make-instance 'dcc-connection
                   :user user
                   :stream (sb-bsd-sockets:socket-make-stream socket :input t :output t :buffering :none)
                   :socket socket
                   :output-stream t))
  #+openmcl
  (let ((socket-stream (ccl:make-socket :remote-host remote-address
                                        :remote-port remote-port)))
    (make-instance 'dcc-connection
                   :user user
                   :stream socket-stream
                   :output-stream output-stream))
  #-(or openmcl sbcl)
  (warn "make-dcc-connection not supported for this implementation."))

(defgeneric dcc-close (connection))
(defgeneric send-dcc-message (connection message))

(defmethod read-message ((connection dcc-connection))
  (let ((message (read-line (dcc-stream connection))))
    (format (output-stream connection) "~A~%" message)
    (force-output (output-stream connection))
    message))

(defmethod read-message-loop ((connection dcc-connection))
  (loop while (read-message connection)))

(defmethod send-dcc-message ((connection dcc-connection) message)
  (format (dcc-stream connection) "~A~%" message))

;; argh.  I want to name this quit but that gives me issues with
;; generic functions.  need to resolve.
(defmethod dcc-close ((connection dcc-connection))
  (close (dcc-stream connection))
  (setf (user connection) nil)
  (setf *dcc-connections* (remove connection *dcc-connections*))
  #+sbcl (sb-bsd-sockets:socket-close (socket connection))
  )

(defmethod connectedp ((connection dcc-connection))
  (let ((stream (dcc-stream connection)))
    (and (streamp stream)
         (open-stream-p stream))))

;;
;; Channel
;;

(defclass channel ()
  ((name
    :initarg :name
    :accessor name)
   (normalized-name
    :initarg :normalized-name
    :accessor normalized-name)
   (topic
    :initarg :topic
    :accessor topic)
   (modes
    :initarg :modes
    :accessor modes
    :initform nil)
   (users
    :initarg :users
    :accessor users
    :initform (make-hash-table :test #'equal))
   (user-count
    :initarg :user-count
    :accessor user-count
    :initform nil
    :documentation "May not represent the real number of users in the
channel.  Rather, the number returned from the LIST command gets stuck
in there so the user of this library can use it for searching
channels, for instance.  If the value is NIL then the slot has not
been populated by a LIST command.")))

(defmethod print-object ((object channel) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (princ (name object) stream)))

(defun normalize-channel-name (connection string)
  "Normalize `string' so that it represents an all-downcased channel
name."
  (irc-string-downcase (case-map-name connection) string))

(defun make-channel (connection
                     &key (name "")
                          (topic "")
                          (modes nil)
                          (users nil)
                          (user-count nil))
  (let ((channel
         (make-instance 'channel
                        :name name
                        :normalized-name
                        (normalize-channel-name connection name)
                        :topic topic
                        :modes modes
                        :user-count user-count)))
    (dolist (user users)
      (add-user channel user))
    channel))

(defgeneric find-channel (connection channel))
(defgeneric remove-all-channels (connection))
(defgeneric add-channel (connection channel))
(defgeneric remove-channel (connection channel))
(defgeneric remove-users (channel))

(defmethod find-channel ((connection connection) (channel string))
  "Return channel as designated by `channel'.  If no such channel can
be found, return nil."
  (let ((channel-name (normalize-channel-name connection channel)))
    (gethash channel-name (channels connection))))

(defmethod remove-all-channels ((connection connection))
  "Remove all channels known to `connection'."
  (clrhash (channels connection)))

(defmethod add-channel ((connection connection) (channel channel))
  "Add `channel' to `connection'."
  (setf (gethash (normalized-name channel) (channels connection)) channel))

(defmethod remove-channel ((connection connection) (channel channel))
  "Remove `channel' from `connection'."
  (remhash (normalized-name channel) (channels connection)))

(defmethod remove-users ((channel channel))
  "Remove all users on `channel'."
  (clrhash (users channel)))

;;
;; User
;;

(defclass user ()
  ((nickname
    :initarg :nickname
    :accessor nickname
    :initform "")
   (normalized-nickname
    :initarg :normalized-nickname
    :accessor normalized-nickname
    :initform "")
   (username
    :initarg :username
    :accessor username
    :initform "")
   (hostname
    :initarg :hostname
    :accessor hostname
    :initform "")
   (realname
    :initarg :realname
    :accessor realname
    :initform "")
   (channels
    :initarg :channels
    :accessor channels
    :initform nil)))

(defmethod print-object ((object user) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A!~A@~A \"~A\""
            (nickname object)
            (username object)
            (hostname object)
            (realname object))))

(defun make-user (connection
                  &key (nickname "")
                       (username "")
                       (hostname "")
                       (realname ""))
  (make-instance 'user
                 :nickname nickname
                 :normalized-nickname (normalize-nickname connection nickname)
                 :username username
                 :hostname hostname
                 :realname realname))

(defun canonicalize-nickname (connection nickname)
  (if (find (char nickname 0)
            (parse-isupport-prefix-argument
             (second (assoc "PREFIX"
                            (server-capabilities connection)
                            :test #'string=))))
      (subseq nickname 1)
      nickname))

(defun normalize-nickname (connection string)
  "Normalize `string' so that represents an all-downcased IRC
nickname."
  (irc-string-downcase (case-map-name connection) string))

(defgeneric find-user (connection nickname))
(defgeneric add-user (object user))
(defgeneric remove-all-users (connection))
(defgeneric remove-user (object user))
(defgeneric remove-user-everywhere (connection user))
(defgeneric find-or-make-user (connection nickname
                                          &key username hostname realname))
(defgeneric change-nickname (connection user new-nickname))

(defmethod find-user ((connection connection) (nickname string))
  "Return user as designated by `nickname' or nil if no such user is
known."
  (let ((nickname (normalize-nickname connection nickname)))
    (or (gethash nickname (users connection))
        (when (string= nickname (nickname (user connection)))
          (user connection)))))

; what if the user is not on any channels?
(defmethod add-user ((connection connection) (user user))
  "Add `user' to `connection'."
  (setf (gethash (normalized-nickname user) (users connection)) user))

(defmethod add-user ((channel channel) (user user))
  (setf (gethash (normalized-nickname user) (users channel)) user)
  (pushnew channel (channels user)))

(defmethod remove-all-users ((connection connection))
  "Remove all users known to `connection'."
  (clrhash (users connection)))

(defmethod remove-user ((channel channel) (user user))
  "Remove `user' from `channel' and `channel' from `user'."
  (remhash (normalized-nickname user) (users channel))
  (setf (channels user) (remove channel (channels user))))

(defmethod remove-channel ((channel channel) (user user))
  "Remove `channel' from `user'."
  (warn
   (concatenate 'string
                "use of depricated API (remove-channel channel user): "
                "(remove-channel user channel) is now preferred"))
  (remove-channel user channel))

(defmethod remove-channel ((user user) (channel channel))
  "Remove `channel' from `user'."
  (setf (channels user) (remove channel (channels user))))

(defmethod remove-user ((connection connection) (user user))
  "Remove `user' from `connection' but leave user in any channels he
may be already be on."
  (remhash (normalized-nickname user) (users connection)))

(defmethod remove-user-everywhere ((connection connection) (user user))
  "Remove `user' anywhere present in the `connection'."
  (dolist (channel (channels user))
    (remove-user channel user))
  (remove-user connection user))

(defmethod find-or-make-user ((connection connection) nickname &key (username "")
                              (hostname "") (realname ""))
  (or (find-user connection nickname)
      (make-user connection
                 :nickname nickname
                 :username username
                 :hostname hostname
                 :realname realname)))

(defmethod change-nickname ((connection connection) (user user) new-nickname)
  (let ((new-user user)
        (channels (channels user)))
    (remove-user connection user)
    (setf (nickname new-user) new-nickname)
    (setf (normalized-nickname new-user)
          (normalize-nickname connection new-nickname))
    (dolist (channel channels)
      (remove-user channel user)
      (add-user channel new-user))
    (add-user connection user)
    new-user))

;; IRC Message
;;

(defclass irc-message ()
  ((source
    :accessor source
    :initarg :source
    :type string)
   (user
    :accessor user
    :initarg :user)
   (host
    :accessor host
    :initarg :host
    :type string)
   (command
    :accessor command
    :initarg :command
    :type string)
   (arguments
    :accessor arguments
    :initarg :arguments
    :type list)
   (trailing-argument
    :accessor trailing-argument
    :initarg :trailing-argument
    :type string)
   (connection
    :accessor connection
    :initarg :connection)
   (received-time
    :accessor received-time
    :initarg :received-time)
   (raw-message-string
    :accessor raw-message-string
    :initarg :raw-message-string
    :type string)))

(defmethod print-object ((object irc-message) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (source object) (command object))))

(defgeneric self-message-p (message))
(defgeneric find-irc-message-class (type))
(defgeneric client-log (connection message &optional prefix))
(defgeneric apply-to-hooks (message))

(defmethod self-message-p ((message irc-message))
  "Did we send this message?"
  (string-equal (source message)
                (nickname (user (connection message)))))

(defclass irc-error-reply (irc-message) ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-message-symbol (prefix name)
    "Intern based on symbol-name to support case-sensitive mlisp"
    (intern
     (concatenate 'string
		  (symbol-name prefix)
		  "-"
		  (symbol-name name)
		  "-"
		  (symbol-name '#:message))))

  (defun define-irc-message (command)
    (let ((name (intern-message-symbol :irc command)))
      `(progn
	(defmethod find-irc-message-class ((type (eql ,command)))
	  (find-class ',name))
	(export ',name)
	(defclass ,name (irc-message) ())))))

(defmacro create-irc-message-classes (class-list)
  `(progn ,@(mapcar #'define-irc-message class-list)))

;; should perhaps wrap this in an eval-when?
(create-irc-message-classes #.(remove-duplicates (mapcar #'second *reply-names*)))
(create-irc-message-classes (:privmsg :notice :kick :topic :error :mode :ping
			     :nick :join :part :quit :kill :pong :invite))

(defmethod find-irc-message-class (type)
  (declare (ignore type))
  (find-class 'irc-message))

(defmethod client-log ((connection connection) (message irc-message) &optional (prefix ""))
  (let ((stream (client-stream connection)))
    (format stream "~A~A: ~A: ~A~{ ~A~} \"~A\"~%"
            prefix
            (received-time message)
            (command message)
            (source message)
            (arguments message)
            (trailing-argument message))
    (force-output stream)))

(defmethod apply-to-hooks ((message irc-message))
  (let ((connection (connection message)))
    (dolist (hook (get-hooks connection (class-name (class-of message))))
      (funcall hook message))))

;;
;; CTCP Message
;;

(defclass ctcp-mixin ()
  ((ctcp-command
    :initarg :ctcp-command
    :accessor ctcp-command)))

(defclass standard-ctcp-message (ctcp-mixin irc-message) ())

(defgeneric find-ctcp-message-class (type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-ctcp-message (ctcp-command)
    (let ((name (intern-message-symbol :ctcp ctcp-command)))
      `(progn
	(defmethod find-ctcp-message-class ((type (eql ,ctcp-command)))
	  (find-class ',name))
	(export ',name)
	(defclass ,name (ctcp-mixin irc-message) ())))))

(defmacro create-ctcp-message-classes (class-list)
  `(progn ,@(mapcar #'define-ctcp-message class-list)))

;; should perhaps wrap this in an eval-when?
(create-ctcp-message-classes (:action :source :finger :ping
                               :version :userinfo :time :dcc-chat-request
                               :dcc-send-request))

(defmethod find-ctcp-message-class (type)
  (declare (ignore type))
  (find-class 'standard-ctcp-message))

(defmethod client-log ((connection connection) (message ctcp-mixin) &optional (prefix ""))
  (let ((stream (client-stream connection)))
    (format stream "~A~A: ~A (~A): ~A~{ ~A~} \"~A\"~%"
            prefix
            (received-time message)
            (command message)
            (ctcp-command message)
            (source message)
            (arguments message)
            (trailing-argument message))
    (force-output stream)))

