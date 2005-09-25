;;;; $Id$
;;;; $Source$

;;;; See LICENSE for licensing information.

(in-package :irc)

(defgeneric irc-message-event (message)
  (:documentation "Upon receipt of an IRC message from the
connection's stream, irc-message-event will be called with the
message."))

(defmethod irc-message-event ((message irc-message))
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
           (target channel-name mask set-by time-set)
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
  (let* ((capabilities (cdr (arguments message)))
         (connection (connection message))
         (current-case-mapping (case-map-name connection)))
    (setf (server-capabilities connection)
          (let ((new-values (mapcar #'(lambda (x)
                                        (let ((eq-pos (position #\= x)))
                                          (if eq-pos
                                              (list (subseq x 0 eq-pos)
                                                    (subseq x (1+ eq-pos)))
                                            (list x)))) capabilities)))
            (merge 'list new-values (copy-seq *default-isupport-values*)
                   #'string= :key #'first)))
    (setf (channel-mode-descriptions connection)
          (chanmode-descs-from-isupport (server-capabilities connection))
          (nick-prefixes connection)
          (nick-prefixes-from-isupport (server-capabilities connection)))
    (when (not (equal current-case-mapping
                      (case-map-name connection)))
      ;; we need to re-normalize nicks and channel names
      (re-apply-case-mapping connection))))

(defmethod default-hook ((message irc-rpl_whoisuser-message))
  (let ((user (find-user (connection message)
                         (second (arguments message))))
        (realname (trailing-argument message))
        (username (third (arguments message)))
        (hostname (fourth (arguments message))))
    (when user
      (setf (realname user) realname)
      (setf (username user) username)
      (setf (hostname user) hostname))))

(defmethod default-hook ((message irc-rpl_list-message))
  (let ((connection (connection message))
        (channel (second (arguments message)))
        (user-count (parse-integer (or (third (arguments message)) "0")))
        (topic (trailing-argument message)))
    (add-channel connection (or (find-channel connection channel)
                                (make-channel connection
                                              :name channel
                                              :topic topic
                                              :user-count user-count)))))

(defmethod default-hook ((message irc-rpl_topic-message))
  (setf (topic (find-channel (connection message)
                             (second (arguments message))))
        (trailing-argument message)))

(defmethod default-hook ((message irc-rpl_namreply-message))
  (let* ((connection (connection message))
         (channel (find-channel connection (car (last (arguments message))))))
    (unless (has-mode-p channel 'namreply-in-progress)
      (add-mode channel 'namreply-in-progress
                (make-instance 'list-value-mode :value-type :user)))
    (dolist (nickname (tokenize-string (trailing-argument message)))
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
                              user))))))))

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
  (pong (connection message) (trailing-argument message)))

(defmethod default-hook ((message irc-join-message))
  (let* ((connection (connection message))
         (user (find-or-make-user
                (connection message)
                (source message)
                :hostname (host message)
                :username (user message)))
         (channel (or (find-channel connection (trailing-argument message))
                      (make-channel connection
                                    :name (trailing-argument message)))))
    (when (self-message-p message)
      (add-channel connection channel))
    (add-user connection user)
    (add-user channel user)))

(defmethod default-hook ((message irc-topic-message))
  (setf (topic (find-channel (connection message)
                             (first (arguments message))))
        (trailing-argument message)))

(defmethod default-hook ((message irc-part-message))
  (let* ((connection (connection message))
         (channel (find-channel connection (first (arguments message))))
         (user (find-user connection (source message))))
    (if (self-message-p message)
        (remove-channel user channel)
        (remove-user channel user))))

(defmethod default-hook ((message irc-quit-message))
  (let ((connection (connection message)))
    (remove-user-everywhere connection (find-user connection (source message)))))

(defmethod default-hook ((message irc-rpl_channelmodeis-message))
  (destructuring-bind
      (target &rest arguments)
      ;; ignore the my own nick which is the first message argument
      (rest (arguments message))
    (let* ((connection (connection message))
           (target (find-channel connection target))
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
  (let* ((con (connection message))
         (user (find-or-make-user con (source message)
                                  :hostname (host message)
                                  :username (user message))))
    (change-nickname con user (trailing-argument message))))

(defmethod default-hook ((message irc-kick-message))
  (let* ((connection (connection message))
         (channel (find-channel connection (first (arguments message))))
         (user (find-user connection (second (arguments message)))))
    (if (self-message-p message)
        (remove-channel user channel)
        (remove-user channel user))))

(defmethod default-hook ((message ctcp-time-message))
  (multiple-value-bind (second minute hour date month year day) (get-decoded-time)
    (send-irc-message
     (connection message)
     :notice (make-ctcp-message
              (format nil "TIME ~A"
                      (make-time-message second minute hour date month year day)))
     (source message))))

(defmethod default-hook ((message ctcp-source-message))
  (send-irc-message
   (connection message)
   :notice (make-ctcp-message
            (format nil "SOURCE ~A:~A:~A"
                    *download-host*
                    *download-directory*
                    *download-file*))
   (source message)))

(defmethod default-hook ((message ctcp-finger-message))
  (let* ((user (user (connection message)))
         (finger-info (if (not (zerop (length (realname user))))
                          (realname user)
                          (nickname user))))
    (send-irc-message
     (connection message)
     :notice (make-ctcp-message
              (format nil "FINGER ~A" finger-info))
     (source message))))

(defmethod default-hook ((message ctcp-version-message))
  (send-irc-message
   (connection message)
   :notice (make-ctcp-message
            (format nil "VERSION ~A" *ctcp-version*))
   (source message)))

(defmethod default-hook ((message ctcp-ping-message))
  (send-irc-message
   (connection message)
   :notice (make-ctcp-message
            (format nil "PING ~A" (trailing-argument message)))
   (source message)))

(defmethod irc-message-event ((message ctcp-dcc-chat-request-message))
  (apply-to-hooks message)
  (client-log (connection message) message))
;  (when (automatically-accept-dcc-connections (configuration (connection message)))
;    (let* ((user (find-user (connection message) (source message)))
;           (args (tokenize-string (trailing-argument message)))
;           (remote-address (hbo-to-vector-quad (parse-integer (fourth args))))
;           (remote-port (parse-integer (fifth args) :junk-allowed t)))
;      (push (make-dcc-connection :user user
;                                 :remote-address remote-address
;                                 :remote-port remote-port)
;            *dcc-connections*))))
  
(defmethod irc-message-event ((message ctcp-dcc-send-request-message))
  (apply-to-hooks message)
  (client-log (connection message) message))
;  (when (automatically-accept-dcc-downloads (configuration (connection message)))
;    (let* ((user (find-user (connection message) (source message)))
;           (args (tokenize-string (trailing-argument message)))
;           (filename (third args))
;           (remote-address (hbo-to-vector-quad (parse-integer (fourth args))))
;           (remote-port (parse-integer (fifth args)))
;           (filesize (parse-integer (sixth args) :junk-allowed t)))
;      (let ((dcc-connection (make-dcc-connection :user user
;                                                 :remote-address remote-address
;                                                 :remote-port remote-port)))
;      (with-open-file (stream filename :direction :output
;                              :if-exists :supersede)
;        (write-sequence (read-message-loop dcc-connection) stream))))))
  
