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
   (server-socket
    :initarg :server-socket
    :accessor server-socket
    :documentation "Socket used to talk to the IRC server.")
   (server-stream
    :initarg :server-stream
    :accessor server-stream
    :documentation "Stream used to talk to the IRC server.")
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

(defun make-connection (&key (user nil)
                             (server-name "")
                             (server-socket nil)
                             (server-stream nil)
                             (client-stream t)
                             (hooks nil))
  (let ((connection (make-instance 'connection
                                   :user user
                                   :server-name server-name
                                   :server-socket server-socket
                                   :server-stream server-stream
                                   :client-stream client-stream)))
    (dolist (hook hooks)
      (add-hook connection (car hook) (cadr hook)))
    connection))

(defmethod add-default-hooks ((connection connection))
  (dolist (message '(irc-rpl_whoisuser-message
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
      (stream-error () (setf read-more-p nil)))))

(defvar *process-count* 0)

(defmethod start-process (function name)
  #+allegro (mp:process-run-function name function)
  #+cmu (mp:make-process function :name name)
  #+lispworks (mp:process-run-function name nil function)
  #+sb-thread (sb-thread:make-thread function))
  
(defmethod start-background-message-handler ((connection connection))
  "Read messages from the `connection', parse them and dispatch
irc-message-event on them. Returns background process ID if available."
  (flet ((do-loop () (read-message-loop connection)))
    (let ((name (format nil "irc-hander-~D" (incf *process-count*))))
      #+(or allegro cmu lispworks sb-thread)
      (start-process #'do-loop name)
      #+(and sbcl (not sb-thread))
      (sb-sys:add-fd-handler (sb-bsd-sockets:socket-file-descriptor
			      (server-socket connection))
			     :input (lambda (fd)
				      (declare (ignore fd))
				      (read-message connection))))))

(defun stop-background-message-handler (process)
  "Stops a background message handler process returned by the start function."
    #+cmu (mp:destroy-process process)
    #+allegro (mp:process-kill process)
    #+sb-thread (sb-thread:destroy-thread process)
    #+lispworks (mp:process-kill process))

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
  #-sbcl
  (warn "make-dcc-connection not supported for this implementation."))

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

(defun normalize-channel-name (string)
  "Normalize `string' so that it represents an all-downcased channel
name."
  (string-downcase string))
  
(defun make-channel (&key (name "")
                          (topic "")
                          (modes nil)
                          (users nil)
                          (user-count nil))
  (let ((channel
         (make-instance 'channel
                        :name name
                        :normalized-name (normalize-channel-name name)
                        :topic topic
                        :modes modes
                        :user-count user-count)))
    (dolist (user users)
      (add-user channel user))
    channel))

(defmethod find-channel ((connection connection) (channel string))
  "Return channel as designated by `channel'.  If no such channel can
be found, return nil."
  (let ((channel-name (normalize-channel-name channel)))
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

(defun make-user (&key (nickname "")
                       (username "")
                       (hostname "")
                       (realname ""))
  (make-instance 'user
                 :nickname nickname
                 :normalized-nickname (normalize-nickname nickname)
                 :username username
                 :hostname hostname
                 :realname realname))

(defun canonicalize-nickname (nickname)
  (if (find (char nickname 0) "@+*")
      (subseq nickname 1)
      nickname))

(defun normalize-nickname (string)
  "Normalize `string' so that represents an all-downcased IRC
nickname."
  (let* ((new-string (substitute #\[ #\{ string))
         (new-string (substitute #\] #\} new-string))
         (new-string (substitute #\\ #\| new-string))
         (new-string (substitute #\~ #\^ new-string)))
    (string-downcase new-string)))

(defmethod find-user ((connection connection) (nickname string))
  "Return user as designated by `nickname' or nil if no such user is
known."
  (let ((nickname (normalize-nickname nickname)))
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
      (make-user :nickname nickname
                 :username username
                 :hostname hostname
                 :realname realname)))

(defmethod change-nickname ((connection connection) (user user) new-nickname)
  (let ((new-user user)
        (channels (channels user)))
    (remove-user connection user)
    (setf (nickname new-user) new-nickname)
    (setf (normalized-nickname new-user) (normalize-nickname new-nickname))
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

(defmethod self-message-p ((message irc-message))
  "Did we send this message?"
  (string-equal (source message)
                (nickname (user (connection message)))))

(defclass irc-error-reply (irc-message) ())

(defmacro define-irc-message (command)
  (let ((*print-case* :upcase))
    (let ((name (intern (format nil "IRC-~A-MESSAGE" command))))
      `(progn
         (defmethod find-irc-message-class ((type (eql ,command)))
           (find-class ',name))
         (export ',name)
         (defclass ,name (irc-message) ())))))

(defun create-irc-message-classes (class-list)
  (dolist (class class-list)
    (eval (list 'define-irc-message class)))) ; argh.  eval.

;; should perhaps wrap this in an eval-when?
(create-irc-message-classes (mapcar #'second *reply-names*))
(create-irc-message-classes '(:privmsg :notice :kick :topic :error
                              :mode :ping :nick :join :part :quit :kill
			      :pong :invite))

(defmethod find-irc-message-class (type)
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

(defclass standard-ctcp-message (ctcp-mixin message) ())

(defmacro define-ctcp-message (ctcp-command)
  (let ((*print-case* :upcase))
    (let ((name (intern (format nil "CTCP-~A-MESSAGE" ctcp-command))))
      `(progn
         (defmethod find-ctcp-message-class ((type (eql ,ctcp-command)))
           (find-class ',name))
         (export ',name)
         (defclass ,name (ctcp-mixin irc-message) ())))))

(defun create-ctcp-message-classes (class-list)
  (dolist (class class-list)
    (eval (list 'define-ctcp-message class)))) ; argh.  eval.  must go away.

;; should perhaps wrap this in an eval-when?
(create-ctcp-message-classes '(:action :source :finger :ping
                               :version :userinfo :time :dcc-chat-request
                               :dcc-send-request))

(defmethod find-ctcp-message-class (type)
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

