;;;; $Id$
;;;; $Source$

;;;; See LICENSE for licensing information.

(in-package :irc)

(defgeneric irc-message-event (connection message)
  (:documentation "Upon receipt of an IRC message from the
connection's stream, irc-message-event will be called with the
message."))

(defmethod irc-message-event (connection (message irc-message))
  (declare (ignore connection))
  (apply-to-hooks message)
  (client-log (connection message) message "UNHANDLED-EVENT:"))


(defgeneric default-hook (message)
  (:documentation "Minimum action to be executed upon reception
of the IRC message to keep the connection, channel and user
objects in sync."))

(defmacro generate-maskmode-hooks (listmsg-class endmsg-class
                                                 tmp-symbol mode-symbol)
  `(progn
     (defmethod default-hook ((message ,listmsg-class))
       (destructuring-bind
           (target channel-name mask &optional set-by time-set)
           (arguments message)
         (declare (ignore target set-by time-set))
         ;; note: the structure currently does not allow for logging
         ;; set-by and time-set: the MODE message handling currently
         ;; does not allow that.
         (let ((channel (find-channel (connection message) channel-name)))
           (when channel
             (unless (has-mode-p channel ',tmp-symbol)
               ;; start with a new list, replacing the old value later
               (add-mode channel ',tmp-symbol
                         (make-instance 'list-value-mode
                                        :value-type :non-user)))
             ;; use package-local symbol to prevent conflicts
             (set-mode channel ',tmp-symbol mask)))))

     (defmethod default-hook ((message ,endmsg-class))
       (let ((channel (find-channel (connection message)
                                    (car (arguments message)))))
         (when channel
           (let ((mode (has-mode-p channel ',tmp-symbol)))
             (when mode
               ;; replace list
               (add-mode channel ',mode-symbol mode)
               (remove-mode channel ',tmp-symbol))))))))

(generate-maskmode-hooks irc-rpl_banlist-message
                         irc-rpl_endofbanlist-message
                         banlist-in-progress :ban)
(generate-maskmode-hooks irc-rpl_exceptlist-message
                         irc-rpl_endofexceptlist-message
                         exceptlist-in-progress :except)
(generate-maskmode-hooks irc-rpl_invitelist-message
                         irc-rpl_endofinvitelist-message
                         invitelist-in-progress :invite)

(defmethod default-hook ((message irc-rpl_isupport-message))
  (destructuring-bind
      (target &rest capabilities)
      ;; the last argument contains only an explanitory text
      (butlast (arguments message))
    (declare (ignore target))
    (let* ((connection (connection message))
           (current-case-mapping (case-map-name connection)))
      (setf (server-capabilities connection)
            (reduce #'(lambda (x y)
                        ;; O(n^2), but we're talking small lists anyway...
                        ;; maybe I should have chosen a hash interface
                        ;; after all...
                        (if (assoc (first y) x :test #'string=)
                            x
                          (cons y x)))
                    (append
                     (mapcar #'(lambda (x)
                                 (let ((eq-pos (position #\= x)))
                                   (if eq-pos
                                       (list (subseq x 0 eq-pos)
                                             (subseq x (1+ eq-pos)))
                                     (list x)))) capabilities)
                     (server-capabilities connection))
                    :initial-value '()))
      (setf (channel-mode-descriptions connection)
            (chanmode-descs-from-isupport (server-capabilities connection))
            (nick-prefixes connection)
            (nick-prefixes-from-isupport (server-capabilities connection)))
      (when (not (equal current-case-mapping
                        (case-map-name connection)))
        ;; we need to re-normalize nicks and channel names
        (re-apply-case-mapping connection)))))

(defmethod default-hook ((message irc-rpl_whoisuser-message))
  (destructuring-bind
      (target nick username hostname star realname)
      (arguments message)
    (declare (ignore target star))
    (let ((user (find-user (connection message) nick)))
      (when user
        (setf (realname user) realname
              (username user) username
              (hostname user) hostname)))))

(defmethod default-hook ((message irc-rpl_list-message))
  (destructuring-bind
      (channel count topic)
      (arguments message)
    (let ((connection (connection message))
          (user-count (parse-integer count)))
      (add-channel connection (or (find-channel connection channel)
                                  (make-channel connection
                                                :name channel
                                                :topic topic
                                                :user-count user-count))))))

(defmethod default-hook ((message irc-rpl_topic-message))
  (destructuring-bind
      (target channel topic)
      (arguments message)
    (declare (ignore target))
    (setf (topic (find-channel (connection message) channel)) topic)))

(defmethod default-hook ((message irc-rpl_namreply-message))
  (let* ((connection (connection message)))
    (destructuring-bind
        (nick chan-mode channel names)
        (arguments message)
      (let ((channel (find-channel connection channel)))
        (unless (has-mode-p channel 'namreply-in-progress)
          (add-mode channel 'namreply-in-progress
                    (make-instance 'list-value-mode :value-type :user)))
        (dolist (nickname (tokenize-string names))
          (let ((user (find-or-make-user connection
                                         (canonicalize-nickname connection
                                                                nickname))))
            (unless (equal user (user connection))
              (add-user connection user)
              (add-user channel user)
              (set-mode channel 'namreply-in-progress user))
            (let* ((mode-char (getf (nick-prefixes connection)
                                    (elt nickname 0)))
                   (mode-name (when mode-char
                                (mode-name-from-char connection
                                                     channel mode-char))))
              (when mode-name
                (if (has-mode-p channel mode-name)
                    (set-mode channel mode-name user)
                  (set-mode-value (add-mode channel mode-name
                                            (make-mode connection
                                                       channel mode-name))
                                  user))))))))))

(defmethod default-hook ((message irc-rpl_endofnames-message))
  (let* ((channel (find-channel (connection message)
                                (second (arguments message))))
         (mode (get-mode channel 'namreply-in-progress))
         (channel-users))
    (remove-mode channel 'namreply-in-progress)
    (maphash #'(lambda (nick user-obj)
                 (declare (ignore nick))
                 (pushnew user-obj channel-users)) (users channel))
    (dolist (user (remove-if #'(lambda (x)
                                 (member x mode)) channel-users))
      (remove-user channel user))))

(defmethod default-hook ((message irc-ping-message))
  (apply #'pong (connection message) (arguments message)))

(defmethod default-hook ((message irc-join-message))
  (with-slots
       (connection source host user arguments)
       message
    (destructuring-bind
        (channel)
        arguments
      (let ((user (find-or-make-user connection source
                                     :hostname host
                                     :username user))
            (channel (or (find-channel connection channel)
                         (make-channel connection :name channel))))
        (when (self-message-p message)
          (add-channel connection channel))
        (add-user connection user)
        (add-user channel user)))))

(defmethod default-hook ((message irc-topic-message))
  (with-slots
       (connection arguments)
       message
    (destructuring-bind
        (channel &optional topic)
        arguments
      (setf (topic (find-channel connection channel)) topic))))

(defmethod default-hook ((message irc-part-message))
  (with-slots
      (connection arguments source)
      message
    (destructuring-bind
        (channel &optional text)
        arguments
      (let ((channel (find-channel connection channel))
            (user (find-user connection source)))
        (when (and user channel)
          (if (self-message-p message)
              (remove-channel user channel)
            (remove-user channel user)))))))

(defmethod default-hook ((message irc-quit-message))
  (let* ((connection (connection message))
         (user (find-user connection (source message))))
    (unless (null user)
      (remove-user-everywhere connection user))))

(defmethod default-hook ((message irc-rpl_channelmodeis-message))
  (with-slots
      (connection arguments)
      message
    (destructuring-bind
        (target channel &rest mode-arguments)
        arguments
    (declare (ignore target))
    (let* ((channel (find-channel connection channel))
           (mode-changes
            (when channel
              (parse-mode-arguments connection channel arguments
                                    :server-p (user connection)))))
      (dolist (change mode-changes)
        (destructuring-bind
            (op mode-name value)
            change
          (unless (has-mode-p channel mode-name)
            (add-mode target mode-name
                      (make-mode connection channel mode-name)))
          (funcall (if (char= #\+ op) #'set-mode #'unset-mode)
                   channel mode-name value)))))))

(defmethod default-hook ((message irc-mode-message))
  (destructuring-bind
      (target &rest arguments)
      (arguments message)
    (let* ((connection (connection message))
           (target (or (find-channel connection target)
                       (find-user connection target)))
           (mode-changes
            (when target
              (parse-mode-arguments connection target arguments
                                     :server-p (user connection)))))
      (dolist (change mode-changes)
        (destructuring-bind
            (op mode-name value)
            change
          (unless (has-mode-p target mode-name)
            (add-mode target mode-name
                      (make-mode connection target mode-name)))
          (funcall (if (char= #\+ op) #'set-mode #'unset-mode)
                   target mode-name value))))))

(defmethod default-hook ((message irc-nick-message))
  (with-slots
      (connection source host user arguments)
      message
    (destructuring-bind
        (new-nick)
        arguments
      (let* ((user (find-or-make-user connection source
                                      :hostname host
                                      :username user)))
        (change-nickname connection user new-nick)))))

(defmethod default-hook ((message irc-kick-message))
  (with-slots
      (connection arguments)
      message
    (destructuring-bind
        (channel nick &optional reason)
        arguments
      (declare (ignore arguments))
      (let* ((channel (find-channel connection channel))
             (user (find-user connection nick)))
        (when (and user channel)
          (if (self-message-p message)
              (remove-channel user channel)
            (remove-user channel user)))))))

(macrolet ((define-ctcp-reply-hook ((message-var message-type) &body body)
               `(defmethod default-hook ((,message-var ,message-type))
                  (when (ctcp-request-p ,message-var)
                    ,@body))))
  (define-ctcp-reply-hook (message ctcp-time-message)
      (multiple-value-bind
          (second minute hour date month year day)
          (get-decoded-time)
        (send-irc-message
         (connection message)
         :notice (source message)
         (make-ctcp-message
          (format nil "TIME ~A"
                  (make-time-message second minute hour date month year day))))))
  (define-ctcp-reply-hook (message ctcp-source-message)
      (send-irc-message
       (connection message)
       :notice
       (source message)
       (make-ctcp-message
        (format nil "SOURCE ~A:~A:~A"
                *download-host*
                *download-directory*
                *download-file*))))
  (define-ctcp-reply-hook (message ctcp-finger-message)
      (let* ((user (user (connection message)))
             (finger-info (if (not (zerop (length (realname user))))
                              (realname user)
                              (nickname user))))
        (send-irc-message
         (connection message)
         :notice (source message)
         (make-ctcp-message
          (format nil "FINGER ~A" finger-info)))))
  (define-ctcp-reply-hook (message ctcp-version-message)
      (send-irc-message
       (connection message)
       :notice (source message)
       (make-ctcp-message
        (format nil "VERSION ~A" *ctcp-version*))))
  (define-ctcp-reply-hook (message ctcp-ping-message)
      (send-irc-message
       (connection message)
       :notice (source message)
       (make-ctcp-message
        (format nil "PING ~A" (car (last (arguments message))))))))

(defmethod irc-message-event (connection (message ctcp-dcc-chat-request-message))
  (declare (ignore connection))
  (apply-to-hooks message)
  (client-log (connection message) message))
;  (when (automatically-accept-dcc-connections (configuration (connection message)))
;    (let* ((user (find-user (connection message) (source message)))
;           (args (tokenize-string (trailing-argument message)))
;           (remote-address (hbo-to-dotted-quad (parse-integer (fourth args))))
;           (remote-port (parse-integer (fifth args) :junk-allowed t)))
;      (push (make-dcc-connection :user user
;                                 :remote-address remote-address
;                                 :remote-port remote-port)
;            *dcc-connections*))))
  
(defmethod irc-message-event (connection (message ctcp-dcc-send-request-message))
  (declare (ignore connection))
  (apply-to-hooks message)
  (client-log (connection message) message))
;  (when (automatically-accept-dcc-downloads (configuration (connection message)))
;    (let* ((user (find-user (connection message) (source message)))
;           (args (tokenize-string (trailing-argument message)))
;           (filename (third args))
;           (remote-address (hbo-to-dotted-quad (parse-integer (fourth args))))
;           (remote-port (parse-integer (fifth args)))
;           (filesize (parse-integer (sixth args) :junk-allowed t)))
;      (let ((dcc-connection (make-dcc-connection :user user
;                                                 :remote-address remote-address
;                                                 :remote-port remote-port)))
;      (with-open-file (stream filename :direction :output
;                              :if-exists :supersede)
;        (write-sequence (read-message-loop dcc-connection) stream))))))
  
