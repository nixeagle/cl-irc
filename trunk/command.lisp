;;;; $Id$
;;;; $Source$

;;;; See LICENSE for licensing information.

(in-package :irc)

(defgeneric pass (connection password))
(defgeneric nick (connection new-nickname))
(defgeneric user- (connection username mode &optional realname))
(defgeneric oper (connection name password))
(defgeneric mode (connection nickname mode))
(defgeneric op (connection channel nickname))
(defgeneric deop (connection channel nickname))
(defgeneric voice (connection channel user))
(defgeneric devoice (connection channel nickname))
(defgeneric ban (connection channel mask))
(defgeneric unban (connection channel mask))
(defgeneric service (connection nickname distribution info))
(defgeneric quit (connection &optional message))
(defgeneric squit (connection server comment))
(defgeneric join (connection channel))
(defgeneric multi-join (connection channels))
(defgeneric part (connection channel))
(defgeneric part-all (connection))
(defgeneric topic- (connection channel topic))
(defgeneric names (connection channel &optional target))
(defgeneric list- (connection &optional channel target))
(defgeneric invite (connection user channel))
(defgeneric kick (connection channel user &optional comment))
(defgeneric privmsg (connection channel message))
(defgeneric notice (connection target message))
(defgeneric motd- (connection &optional target))
(defgeneric lusers (connection &optional mask target))
(defgeneric version (connection &optional target))
(defgeneric stats (connection &optional query target))
(defgeneric links (connection &optional remote-server server-mask))
(defgeneric time- (connection &optional target))
(defgeneric trace- (connection &optional target))
(defgeneric admin (connection &optional target))
(defgeneric info (connection &optional target))
(defgeneric servlist (connection &optional mask type))
(defgeneric squery (connection service-name text))
(defgeneric who (connection &optional mask o))
(defgeneric whois (connection mask &optional target))
(defgeneric whowas (connection nickname &optional count target))
(defgeneric kill (connection user &optional comment))
(defgeneric ping (connection server))
(defgeneric pong (connection server &optional server2))
(defgeneric error- (connection message))
(defgeneric away (connection message))
(defgeneric rehash (connection))
(defgeneric die (connection))
(defgeneric restart- (connection))
(defgeneric summon (connection nickname &optional target channel))
(defgeneric users- (connection &optional target))
(defgeneric wallops (connection message))
(defgeneric userhost (connection nickname))
(defgeneric ison (connection user))
(defgeneric ctcp (connection target message))
(defgeneric ctcp-chat-initiate (connection nickname))


(defmethod pass ((connection connection) (password string))
  "A \"PASS\" command is not required for a client connection to be
registered, but it MUST precede the latter of the NICK/USER
combination (for a user connection) or the SERVICE command (for a
service connection). The RECOMMENDED order for a client to register is
as follows:

                           1. Pass message
           2. Nick message                 2. Service message
           3. User message

Upon success, the client will receive an RPL_WELCOME (for users) or
RPL_YOURESERVICE (for services) message indicating that the connection
is now registered and known the to the entire IRC network.  The reply
message MUST contain the full client identifier upon which it was
registered."
  (send-irc-message connection :pass nil password))

(defmethod nick ((connection connection) (new-nickname string))
  (send-irc-message connection :nick nil new-nickname))

(defmethod user- ((connection connection) (username string)
                  (mode integer) &optional (realname ""))
  (send-irc-message connection :user realname username mode "*"))

(defmethod oper ((connection connection) (name string) (password string))
  (send-irc-message connection :oper nil name password))

(defmethod mode ((connection connection) (nickname string) (mode string))
  (send-irc-message connection :mode nil nickname mode))

;; utility functions not part of the RFCs
(defmethod op ((connection connection) (channel string) (nickname string))
  (send-irc-message connection :mode nil channel "+o" nickname))

(defmethod op ((connection connection) (channel channel) (user user))
  (op connection (name channel) (nickname user)))

(defmethod deop ((connection connection) (channel string) (nickname string))
  (send-irc-message connection :mode nil channel "-o" nickname))

(defmethod deop ((connection connection) (channel channel) (user user))
  (deop connection (name channel) (nickname user)))

(defmethod voice ((connection connection) (channel string) (nickname string))
  (send-irc-message connection :mode nil channel "+v" nickname))

(defmethod voice ((connection connection) (channel channel) (user user))
  (voice connection (name channel) (nickname user)))

(defmethod devoice ((connection connection) (channel string) (nickname string))
  (send-irc-message connection :mode nil channel "-v" nickname))

(defmethod devoice ((connection connection) (channel channel) (user user))
  (devoice connection (name channel) (nickname user)))

(defmethod ban ((connection connection) (channel string) (mask string))
  (send-irc-message connection :mode nil channel "+b" mask))

(defmethod ban ((connection connection) (channel channel) (mask string))
  (ban connection (name channel) mask))

;; unban or deban?
(defmethod unban ((connection connection) (channel string) (mask string))
  (send-irc-message connection :mode nil channel "-b" mask))

(defmethod unban ((connection connection) (channel channel) (mask string))
  (unban connection (name channel) mask))

(defmethod service ((connection connection) (nickname string)
                    (distribution string) (info string))
  (send-irc-message connection :service info nickname "*" distribution 0 0 info))

(defmethod quit ((connection connection) &optional (message *default-quit-message*))
  (remove-all-channels connection)
  (remove-all-users connection)
  (unwind-protect
      (send-irc-message connection :quit message)
    #+(and sbcl (not sb-thread))
    (sb-sys:invalidate-descriptor (sb-sys:fd-stream-fd
                                   (server-stream connection)))
    (close (server-stream connection))))

(defmethod squit ((connection connection) (server string) (comment string))
  (send-irc-message connection :squit comment server))

(defmethod join ((connection connection) (channel string))
  (send-irc-message connection :join nil channel))

(defmethod join ((connection connection) (channel channel))
  (join connection (name channel)))

;; utility function not part of the RFC
(defmethod multi-join ((connection connection) (channels list))
  (dolist (channel channels)
    (join connection channel)))

(defmethod part ((connection connection) (channel string))
  (send-irc-message connection :part nil channel))

(defmethod part ((connection connection) (channel channel))
  (part connection (name channel)))

;; utility function not part of the RFC
(defmethod part-all ((connection connection))
  (dolist (channel (channels connection))
    (part connection (name channel))))

(defmethod topic- ((connection connection) (channel string) (topic string))
  (send-irc-message connection :topic topic channel))

(defmethod topic- ((connection connection) (channel channel) (topic string))
  (topic- connection (name channel) topic))

(defmethod names ((connection connection) (channel string)
                  &optional (target ""))
  (send-irc-message connection :names nil channel target))

(defmethod names ((connection connection) (channel channel)
                  &optional (target ""))
  (names connection (name channel) target))

(defmethod list- ((connection connection) &optional
                  (channel "") (target ""))
  (send-irc-message connection :list nil channel target))

(defmethod invite ((connection connection) (nickname string) (channel string))
  (send-irc-message connection :invite nil nickname channel))

(defmethod invite ((connection connection) (user user) (channel channel))
  (invite connection (nickname user) (name channel)))

(defmethod kick ((connection connection) (channel string)
                 (user string) &optional (comment ""))
  (send-irc-message connection :kick comment channel user))

(defmethod kick ((connection connection) (channel channel)
                 (user user) &optional (comment ""))
  (kick connection (name channel) (nickname user) comment))

(defmethod privmsg ((connection connection) (target string) (message string))
  (send-irc-message connection :privmsg message target))

(defmethod privmsg ((connection connection) (user user) (message string))
  (privmsg connection (nickname user) message))

(defmethod privmsg ((connection connection) (channel channel) (message string))
  (privmsg connection (name channel) message))

(defmethod notice ((connection connection) (target string) (message string))
  (send-irc-message connection :notice message target))

(defmethod notice ((connection connection) (user user) (message string))
  (notice connection (nickname user) message))

(defmethod notice ((connection connection) (channel channel) (message string))
  (notice connection (name channel) message))

(defmethod motd- ((connection connection) &optional (target ""))
  (send-irc-message connection :motd nil target))

(defmethod lusers ((connection connection) &optional (mask "") (target ""))
  (send-irc-message connection :lusers nil mask target))

(defmethod version ((connection connection) &optional (target ""))
  (send-irc-message connection :version nil target))

(defmethod stats ((connection connection) &optional (query "") (target ""))
  (send-irc-message connection :stats nil query target))

(defmethod links ((connection connection) &optional (remote-server "")
                  (server-mask ""))
  (send-irc-message connection :links nil remote-server server-mask))

(defmethod time- ((connection connection) &optional (target ""))
  (send-irc-message connection :time nil target))

(defun connect (&key (nickname *default-nickname*)
                     (username nil)
                     (realname nil)
                     (password nil)
                     (mode 0)
                     (server *default-irc-server*)
                     (port *default-irc-server-port*)
                     (connection-type 'connection)
                     (logging-stream t))
  "Connect to server and return a connection object."
  (let* ((stream (socket-connect server port))
         (connection (make-connection :connection-type connection-type
                                      :server-stream stream
                                      :client-stream logging-stream
                                      :server-name server))
         (user (make-user connection
                          :nickname nickname
                          :username username
                          :realname realname)))
    (setf (user connection) user)
    (unless (null password)
      (pass connection password))
    (nick connection nickname)
    (user- connection (or username nickname) mode (or realname nickname))
    (add-default-hooks connection)
    connection))

(defmethod trace- ((connection connection) &optional (target ""))
  (send-irc-message connection :trace nil target))

(defmethod admin ((connection connection) &optional (target ""))
  (send-irc-message connection :admin nil target))

(defmethod info ((connection connection) &optional (target ""))
  (send-irc-message connection :info nil target))

(defmethod servlist ((connection connection) &optional (mask "") (type ""))
  (send-irc-message connection :servlist nil mask type))

(defmethod squery ((connection connection) (service-name string) (text string))
  (send-irc-message connection :squery text service-name))

(defmethod who ((connection connection) &optional (mask "") (o ""))
  (send-irc-message connection :who nil mask o))

(defmethod whois ((connection connection) (mask string) &optional (target ""))
  (send-irc-message connection :whois nil target mask))

(defmethod whowas ((connection connection) (nickname string)
                   &optional (count "") (target ""))
  (send-irc-message connection :whowas nil nickname count target))

(defmethod kill ((connection connection) (nickname string) &optional (comment ""))
  (send-irc-message connection :kill comment nickname))

(defmethod kill ((connection connection) (user user) &optional (comment ""))
  (kill connection (nickname user) comment))

(defmethod ping ((connection connection) (server string))
  (send-irc-message connection :ping nil server))

(defmethod pong ((connection connection) (server string) &optional (server2 ""))
  (send-irc-message connection :pong nil server server2))

(defmethod error- ((connection connection) (message string))
  (send-irc-message connection :error message))

(defmethod away ((connection connection) (message string))
  (send-irc-message connection :away message))

(defmethod rehash ((connection connection))
  (send-irc-message connection :rehash))

(defmethod die ((connection connection))
  (send-irc-message connection :die))

(defmethod restart- ((connection connection))
  (send-irc-message connection :restart))

(defmethod summon ((connection connection) (nickname string)
                   &optional (target "") (channel ""))
  (send-irc-message connection :summon nil nickname target channel))

(defmethod users- ((connection connection) &optional (target ""))
  (send-irc-message connection :users nil target))

(defmethod wallops ((connection connection) (message string))
  (send-irc-message connection :wallops message))

(defmethod userhost ((connection connection) (nickname string))
  (send-irc-message connection :userhost nil nickname))

(defmethod userhost ((connection connection) (user user))
  (userhost connection (nickname user)))

(defmethod ison ((connection connection) (nickname string))
  (send-irc-message connection :ison nil nickname))

(defmethod ison ((connection connection) (user user))
  (ison connection (nickname user)))

;; utility functions not part of the RFC
(defmethod ctcp ((connection connection) target message)
  (send-irc-message connection :privmsg (make-ctcp-message message) target))

(defmethod ctcp-chat-initiate ((connection connection) (nickname string))
  #+sbcl
  (let ((socket (sb-bsd-sockets:make-inet-socket :stream :tcp))
        (port 44347))
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port) ; arbitrary port
    (sb-bsd-sockets:socket-listen socket 1) ; accept one connection
    (ctcp connection nickname
          (format nil "DCC CHAT chat ~A ~A"
                                        ; the use of hostname here is incorrect (it could be a firewall's IP)
                  (host-byte-order (hostname (user connection))) port))
    (make-dcc-connection :user (find-user connection nickname)
                         :input-stream t
                         :output-stream (sb-bsd-sockets:socket-make-stream socket :input t :output t :buffering :none)
                         :socket socket))
  #-sbcl (warn "ctcp-chat-initiate is not supported on this implementation.")
  )
