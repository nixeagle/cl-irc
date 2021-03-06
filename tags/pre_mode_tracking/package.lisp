;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

;; the exports list needs some cleanup/clarification/categorization
(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :cl-irc
      (:use :cl)
    (:nicknames :irc)
    (:export :read-message-loop
             :read-message
             :start-background-message-handler
             :stop-background-message-handler
             :socket-connect
             :send-message
             :server-name
             :no-such-reply
             :parse-raw-message
             :normalize-nickname
             :normalize-channel-name
             :name
             :normalized-name
             :topic
             :modes
             :user-count
             :users
             :server-stream
             :client-stream
             :channels
             :configuration
             :dangling-users
             :channel-list
             :add-hook
             :remove-hook
             :remove-hooks
             :remove-all-hooks
             :add-default-hooks
             :get-hooks
             :make-user
             :nickname
             :normalized-nickname
             :username
             :hostname
             :realname
             :change-nickname
             :irc-message
             :source
             :user
             :host
             :command
             :arguments
             :trailing-argument
             :connection
             :received-time
             :raw-message-string
             :make-connection
             :make-channel
             :channel
             :client-log
             :find-channel
             :find-reply-name
             :remove-channel
             :remove-all-channels
             :add-channel
             :find-user
             :add-user
             :remove-all-users
             :remove-user
             :self-message-p
             :pass
             :nick
             :user-
             :oper
             :mode
             :op
             :deop
             :voice
             :devoice
             :ban
             :unban
             :service
             :quit
             :squit
             :join
             :part
             :part-all
             :topic-
             :names
             :list-
             :invite
             :kick
             :privmsg
             :notice
             :motd-
             :lusers
             :version
             :stats
             :links
             :time-
             :connect
             :trace-
             :admin
             :info
             :servlist
             :squery
             :who
             :whois
             :whowas
             :kill
             :ping
             :pong
             :error-
             :away
             :rehash
             :die
             :restart-
             :summon
             :users-
             :wallops
             :userhost
             :ison)))

