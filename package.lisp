;;; --------------------------------------------------------------------
;;; CLAIM -- An implementation of the AIM TOC protocol in Lisp.
;;;
;;; Copyright 2001, 2002, 2004 I/NET Inc. (http://www.inetmi.com/)
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2004-10-29
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; Package definition.

(cl:defpackage #:com.lemonodor.claim
  (:use :common-lisp)
  (:export 
   #:aim-connection
   #:open-aim-connection
   #:close-aim-connection

   #:aim-connection-username
   #:aim-connection-last-error
   
   #:receive-event
   #:handle-event
   #:receive-events

   #:do-send-im
   #:do-add-buddies
   #:do-remove-buddies
   #:do-add-permits
   #:do-add-denies
   #:do-warn
   #:do-set-config
   #:do-set-idle-time
   #:do-set-away-message
   #:do-get-info
   #:do-set-info
   #:do-chat-invite
   #:do-chat-accept
   #:do-chat-leave
   #:do-chat-whisper
   #:do-chat-send
   #:do-chat-join
   #:do-get-dir
   #:do-set-dir
   #:do-dir-search

   #:handle-client-event
   #:handle-im-in
   #:handle-update-buddy
   #:handle-warned
   #:handle-chat-join
   #:handle-chat-in
   #:handle-chat-update-buddy
   #:handle-chat-invite
   #:handle-chat-left
   #:handle-goto-url
   #:handle-pause
   #:handle-bart
   #:handle-buddy-capabilities
   #:handle-buddy-list-buddy-addition
   #:handle-buddy-list-buddy-deletion
   #:handle-buddy-list-group-deletion
   #:handle-buddy-list-group-addition
   #:handle-permit-list-deletion
   #:handle-permit-list-addition
   #:handle-buddy-update
   #:handle-deny-list-addition
   #:handle-deny-list-deletion
   #:handle-new-buddy-reply
   #:handle-update
   #:handle-unknown-event
   #:handle-nick

   #:im-text
   #:im-user
   #:im-auto-p
   #:im-encoding
   #:im-buddy-status
   
   #:*default-toc-host*
   #:*default-toc-port*
   #:*default-login-host*
   #:*default-language*
   #:*default-agent*
   
   #:disconnect-error))
