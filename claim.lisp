;;; --------------------------------------------------------------------
;;; CLAIM -- An implementation of the AIM TOC protocol in Lisp.
;;;
;;; Copyright 2001, 2002, 2004 I/NET Inc. (http://www.inetmi.com/)
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2005-01-06
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; The heart of the code.

(in-package :com.lemonodor.claim)

   
(defparameter *default-toc-host* "toc.oscar.aol.com")
(defparameter *default-toc-port* 9898)
(defparameter *default-login-host* "login.oscar.aol.com")
(defparameter *default-login-port* 5190)
(defparameter *default-language* "english")
(defparameter *default-agent* "CLAIM 1.1")

(defparameter *external-format* :latin-1)

(defclass aim-connection ()
  (
   ;; Connection parameters
   (toc-host
    :accessor aim-connection-toc-host
    :initarg :toc-host
    :initform *default-toc-host*)
   (toc-port
    :accessor aim-connection-toc-port
    :initarg :toc-port
    :initform *default-toc-port*)
   (login-host
    :accessor aim-connection-login-host
    :initarg :login-host
    :initform *default-login-host*)
   (login-port
    :accessor aim-connection-login-port
    :initarg :login-port
    :initform *default-login-port*)
   (language
    :accessor aim-connection-language
    :initarg :language
    :initform *default-language*)
   (agent
    :accessor aim-connection-agent
    :initarg :agent
    :initform *default-agent*)
   (username
    :accessor aim-connection-username
    :initarg :username)
   (password
    :accessor aim-connection-password
    :initarg :password)
   (profile
    :accessor aim-connection-profile
    :initarg :profile
    :initform nil)
   ;; Once it's connected
   (stream
    :accessor aim-connection-stream
    :initform nil)
   (last-error
    :accessor aim-connection-last-error
    :initform nil)
   (running-p
    :accessor running-p
    :initform T)
   (ignore-events-p
    :accessor ignore-events-p
    :initform NIL)

   ;; Automatic reconnection
   (auto-reconnect-p
    :accessor aim-connection-auto-reconnect-p :initarg :auto-reconnect-p)
   (last-disconnection-time
    :accessor aim-connection-last-disconnection-time
    :initform nil)
   (disconnection-count
    :accessor aim-connection-disconnection-count
    :initform 0))
  (:default-initargs
   :profile "You're talking to the Common Lisp TOC interface, by John Wiseman (jjwiseman@yahoo.com)"
   :auto-reconnect-p T))


(defclass im ()
  ((user :initarg :user :reader im-user)
   (encoding :initarg :encoding :reader im-encoding)
   (text :initarg :text :reader im-text)))

(defclass personal-im (im)
  ((auto-response-p :initarg :auto-response-p :reader im-auto-response-p)
   (buddy-status :initarg :buddy-status :reader im-buddy-status-p)
   (unk1 :initarg :unk1)
   (unk2 :initarg :unk2)
   (unk3 :initarg :unk3)
   (unk4 :initarg :unk4)))

(defclass chat-im (im)
  ((chatroom-id :initarg :chatroom-id :reader im-chatroom-id)
   (whisper-p :initarg :whisper-p :reader im-whisper-p)
   (unk1 :initarg :unk1)
   (unk2 :initarg :unk2)
   (unk3 :initarg :unk3)
   (unk4 :initarg :unk4)))

(defmethod trace-log ((self aim-connection) fmt &rest args)
  (format T "~&~S ~S: ~A" (get-universal-time) self (apply #'format NIL fmt args)))


(defun crlf (stream)
  (write-sequence #(13 10) stream))

(defmethod open-aim-connection ((self aim-connection))
  (when (aim-connection-stream self)
    (trace-log self "Closing previous connection to server.")
    (close (aim-connection-stream self)))
  (handler-case
   (let ((socket (open-socket self)))
     (setf (aim-connection-last-error self) nil)
     (setf (aim-connection-stream self) socket)
     (trace-log self "Sending FLAPON to server.")
     (%send-string socket (format nil "FLAPON"))
     (crlf socket)
     (crlf socket)
     (finish-output socket))
   (error (e)
	  (trace-log self "Got connection error ~S" e)
	  (error 'disconnect-error :original-error e)))
  (values))

(defmethod close-aim-connection ((self aim-connection))
  (when (aim-connection-stream self)
    (trace-log self "Closing connection to server.")
    (close (aim-connection-stream self))
    (setf (aim-connection-stream self) nil)))

(defmethod open-socket ((self aim-connection))
  (trace-log self "Opening socket to TOC server.")
  (let ((raw-stream (or (make-tcp-socket (aim-connection-toc-host self)
					 (aim-connection-toc-port self))
			(error "Unable to connect."))))
    (flexi-streams:make-flexi-stream raw-stream :external-format *external-format*)))


(defstruct toc-event
  type
  payload
  sequence-number)

(defmethod receive-event ((self aim-connection))
  (trace-log self "Waiting for event.")
  (multiple-value-bind (payload type sequence-number)
      (read-flap (aim-connection-stream self))
    (make-toc-event :type type :payload payload :sequence-number sequence-number)))

(defmethod handle-event ((self aim-connection) event)
  (let ((type (toc-event-type event))
	(payload (toc-event-payload event)))
    (trace-log self "Handling event of type ~S" type)
    (ecase type
      (:sign-on (handle-flap-sign-on self payload))
      (:data (toc-handle-receive self payload))
      (:sign-off (handle-flap-sign-off self payload))
      (:keep-alive (handle-flap-keep-alive self payload)))))

(defmethod receive-would-block-p ((self aim-connection))
  (let* ((stream (aim-connection-stream self))
	 (char (read-char-no-hang stream NIL :eof)))
    (when (and char (not (eq char :eof)))
      (unread-char char stream))
    (and char T)))

(defmethod receive-events ((self aim-connection))
  (loop while (running-p self)
	do
	(let ((event (receive-event self)))
	  (unless (ignore-events-p self)
	    (handle-event self event)))))


;; ------------------------------------------------
;; Events
;; ------------------------------------------------

(defmethod handle-flap-sign-on ((self aim-connection) version)
  (declare (ignore version))
  (send-flap-sign-on self)
  (send-toc-sign-on self))

(defmethod handle-flap-sign-off ((self aim-connection) version)
  (declare (ignore version)))

(defmethod handle-flap-keep-alive ((self aim-connection) version)
  (declare (ignore version)))
  
(defmethod handle-toc-sign-on ((self aim-connection) version)
  (declare (ignore version))
  ;;(send-toc-sign-on self)
  ;; Required for proper initialization.
  (send-toc self "toc_add_buddy ~A" (encode-string (normalize-string (aim-connection-username self))))
  (when (aim-connection-profile self)
    (do-set-info self (aim-connection-profile self)))
  (handle-pre-init-done self)
  (send-toc-init-done self))


(defmethod handle-pre-init-done ((self aim-connection))
  "Initial permit/deny items should be sent after receiving SIGN_ON
but before sending toc_init_done, otherwise the user will flash on
peoples buddylist who the user has denied.  handle-pre-init-done is
called just before toc_init_done is sent.  You may also want to
send the toc_add_buddies at this time."
  )
   
(defmethod handle-toc-config ((self aim-connection) config)
  (declare (ignore config)))

(defmethod send-toc-init-done ((self aim-connection))
  (send-toc self "toc_init_done"))

(defmethod handle-nick ((self aim-connection) nick)
  (declare (ignore nick)))
(defmethod handle-im-in ((self aim-connection) personal-im)
  (declare (ignore personal-im)))
(defmethod handle-buddy-update ((self aim-connection) nick online warn sign-on idle away unk1)
  (declare (ignore nick online warn sign-on idle away unk1)))
(defmethod handle-buddy-capabilities ((self aim-connection) nick capabilities)
  (declare (ignore nick capabilities)))
(defmethod handle-bart ((self aim-connection) nick bart-string)
  (declare (ignore nick bart-string)))
(defmethod handle-client-event ((self aim-connection) buddy status)
  (declare (ignore buddy status)))

(defmethod handle-update ((self aim-connection) b nick unk1 alias)
  (declare (ignore b nick unk1 alias)))


(defmethod handle-buddy-list-group-addition ((self aim-connection) group-name)
  (declare (ignore group-name)))
(defmethod handle-buddy-list-buddy-addition ((self aim-connection) alias username group)
  (declare (ignore alias username group)))
(defmethod handle-deny-list-addition ((self aim-connection) nick)
  (declare (ignore nick)))
(defmethod handle-permit-list-addition ((self aim-connection) nick)
  (declare (ignore nick)))
(defmethod handle-buddy-list-group-deletion ((self aim-connection) group-name)
  (declare (ignore group-name)))
(defmethod handle-buddy-list-buddy-deletion ((self aim-connection) alias username group)
  (declare (ignore alias username group)))
(defmethod handle-deny-list-deletion ((self aim-connection) nick)
  (declare (ignore nick)))
(defmethod handle-permit-list-deletion ((self aim-connection) nick)
  (declare (ignore nick)))

(defmethod handle-toc-error ((self aim-connection) code args)
  (warn "AIM error: ~A" (error-string code args)))


(defvar *error-strings*
    '(;; ----- General errors
      901 "~A not currently available."
      902 "Warning of ~A not currently available."
      903 "A message has been dropped, you are exceeding the server speed limit."
      ;; ----- Chat errors
      950 "Chat in ~A is unavailable."
      ;; ----- IM & info errors
      960 "You are sending message too fast to ~A."
      961 "You missed an IM from ~A because it was too big."
      962 "You missed an IM from ~A because it was sent too fast."
      ;; ----- Directory errors
      970 "Failure."
      971 "Too many matches."
      972 "Need more qualifiers."
      973 "Dir service temporarily unavailable."
      974 "Email lookup restricted."
      975 "Keyword Ignored."
      976 "No Keywords."
      977 "Language not supported."
      978 "Country not supported."
      979 "Failure unknown: ~A."
      ;; ----- Authorization errors
      980 "Incorrect nickname or password."
      981 "The service is temporarily unavailable."
      982 "Your warning level is currently too high to sign on."
      983 "You have been connecting and disconnecting too frequently.  Wait 10 minutes and try again.  If you continue to try, you will need to wait even longer."
      989 "An unknown signon error has occurred: ~A"))

(defun error-string (code args)
  (let ((format (getf *error-strings* code)))
    (if format
      (apply #'format nil format args)
      nil)))
       
(defmethod handle-warned ((self aim-connection) warn-level warner)
  (declare (ignore warn-level warner)))
(defmethod handle-chat-join ((self aim-connection) room-id room)
  (declare (ignore room-id room)))
(defmethod handle-chat-in ((self aim-connection) chat-im)
  (declare (ignore chat-im)))
(defmethod handle-chat-update-buddy ((self aim-connection) room-id inside-p users)
  (declare (ignore room-id inside-p users)))
(defmethod handle-chat-invite ((self aim-connection) room room-id sender message)
  (declare (ignore room room-id sender message)))
(defmethod handle-chat-left ((self aim-connection) room-id)
  (declare (ignore room-id)))
(defmethod handle-goto-url ((self aim-connection) window-id url)
  (declare (ignore window-id url)))
(defmethod handle-pause ((self aim-connection)))

(defmethod handle-new-buddy-reply ((self aim-connection) buddy action)
  (declare (ignore buddy action)))

(defvar *min-connection-threshold* 600)
(defvar *max-reconnection-attempts* 10)
    
(defmethod handle-disconnection ((self aim-connection))
  (let ((now (get-universal-time))
	(last-time (or (aim-connection-last-disconnection-time self) 0)))
    (setf (aim-connection-last-disconnection-time self) now)
    (when (> (- now last-time) *min-connection-threshold*)
      (setf (aim-connection-disconnection-count self) 0))
    (labels ((reconnect ()
	       (let ((num-attempts (incf (aim-connection-disconnection-count self))))
		 (if (> num-attempts *max-reconnection-attempts*)
		   (progn
		     (warn "Made ~S attempts to reconnect to AIM; giving up." (- num-attempts 1))
		     NIL)
		   (let ((delay (backoff-time num-attempts)))
		     (warn "Waiting ~S seconds before trying to reconnect to AIM." delay)
		     (sleep delay)
		     (setf (aim-connection-last-disconnection-time self) (get-universal-time))
		     (handler-case
			 (progn
			   (open-aim-connection self)
			   (warn "Connected.")
			   T)
		       (error (e)
			 (warn "While trying to reconnect to AIM got socket error ~A." e)
			 (reconnect))))))))
      (reconnect))))

;; Returns the number of seconds to wait before making a reconnection
;; attempt, using the Truncated Binary Exponential Backoff algorithm
;; (http://www.iol.unh.edu/testsuites/ge/mac/test_4.6.2.html).
;;
;; The time is random; the maximum is about 1 hour after 6 or
;; more attempts.

(defvar *backoff-time-scale* 60)
(defun backoff-time (attempt-number)
  (let* ((k (min 6 attempt-number))
	 (limit (expt 2 k)))
    (random (* limit *backoff-time-scale*))))


(defmacro with-toc-args ((toc-str &optional next-fn rest-fn rest-string-fn) &body body)
  (setf next-fn (or next-fn 'next-toc-arg))
  (setf rest-fn (or rest-fn 'rest-toc-args))
  (setf rest-string-fn (or rest-string-fn 'rest-toc-string))
  (let ((index-var (gensym "INDEX"))
	(string-var (gensym "TOC-STRING")))
    `(let ((,index-var 0)
	   (,string-var ,toc-str))
       (labels ((,next-fn () (if (null ,index-var)
			       nil
			       (multiple-value-bind (arg next)
				   (parse-toc-arg ,string-var :start ,index-var)
				 (setf ,index-var next)
				 arg)))
		(,rest-fn () (let ((args '()))
			       (do ((arg (,next-fn) (,next-fn)))
				   ((null arg) (reverse args))
				 (push arg args))))
		(,rest-string-fn () (subseq ,string-var ,index-var)))
	 ,@body))))

(defun parse-toc-arg (message &key (start 0))
  (next-delimited-token message #\: :start start))


(defmacro stringcase (string-key &rest clauses)
  (flet ((str (x)
	   (unless (stringp x)
	     (warn "~S is not a string literal in stringcase." x))
	   x))
    (let ((string-var (gensym)))
      `(let ((,string-var ,string-key))
	 (cond
	  ,@(mapcar #'(lambda (clause)
			(cond ((or (eq (car clause) 'otherwise)
				   (eq (car clause) T))
			       `(T ,@(cdr clause)))
			      ((not (listp (car clause)))
			       `((string= ,string-var ,(str (car clause)))
				 ,@(cdr clause)))
			      (T
			       `((or ,@(mapcar #'(lambda (val)
						   `(string= ,string-var ,(str val)))
					       (car clause)))
				 ,@(cdr clause)))))
		    clauses))))))

(defmethod toc-handle-receive ((self aim-connection) string)
  (with-toc-args (string)
    (let ((command (next-toc-arg)))
      (trace-log self "Handling command ~S" string)
      (stringcase command

	;; SIGN_ON:<client version supported>
        ;; This is sent after a successful toc_signon command is sent
	;; to TOC. If the command was unsuccessful either the FLAP
	;; connection will be dropped or you will receive a ERROR
	;; message.
       ("SIGN_ON"
	(handle-toc-sign-on self (next-toc-arg)))

       ;; CONFIG2:<config>
       ;; A user's config. Config can be empty in which case the host
       ;; was not able to retrieve it, or a config didn't exist for
       ;; the user.  See the description of toc_set_config in
       ;; PROTOCOL.txt for the format, with changes for TOC2 noted in
       ;; TOC2.txt.
       ("CONFIG2"
	(handle-toc-config self (next-toc-arg)))

       ;; NEW_BUDDY_REPLY2:<buddy>:<action>
       ;; This shows up after you add a buddy. The action can be
       ;; either "added", which means that the buddy was added
       ;; correctly, or "auth" which is used in ICQ to siginify that
       ;; that user has requested authorization to you to their buddy
       ;; list.
       ("NEW_BUDDY_REPLY2"
	(handle-new-buddy-reply self
				(next-toc-arg)
				(stringcase (next-toc-arg)
				  ("added" :added)
				  ("auth" :auth))))
				
       ;; NICK:<nickname>
       ;; Tells you your correct nickname (ie how it should be
       ;; capitalized and spacing)
       ("NICK"
	(handle-nick self (next-toc-arg)))

       ;; IM_IN_ENC2:<user>:<auto>:<???>:<???>:<buddy status>:<???>:<???>:en:<message>
       ;; This command received instead of IM_IN. It is similar to TOC
       ;; 1.0 except there are a few new parameters. One of them is
       ;; language and another is the buddy status, but the rest are
       ;; unknown.
       ("IM_IN_ENC2"
	(handle-im-in self
		      (make-instance 'personal-im
				     :user (next-toc-arg)
				     :auto-response-p (string= "T" (next-toc-arg))
				     :unk1 (next-toc-arg)
				     :unk2 (next-toc-arg)
				     :buddy-status (next-toc-arg)
				     :unk3 (next-toc-arg)
				     :unk4 (next-toc-arg) 
				     :encoding (next-toc-arg)
				     :text (rest-toc-string))))

       ;; UPDATE_BUDDY2:<screenname>:<online? T/F>:<evil amount>:<signon time>:<idle time>:<user class>:<???>
       ;;
       ;; This one command handles arrival/depart/updates.  Evil
       ;; amount is a percentage, signon time is UNIX epoch, idle time
       ;; is in minutes, user class is a two character string:
       ;;
       ;;   uc[0]:
       ;;     ' '  - Ignore
       ;;     'A'  - On AOL
       ;;   uc[1]
       ;;     ' '  - Ignore
       ;;     'A'  - Oscar Admin
       ;;     'U'  - Oscar Unconfirmed
       ;;     'O'  - Oscar Normal
       ;;
       ;; Same as TOC1.0 except there's a mystery parameter.
       ("UPDATE_BUDDY2"
	(handle-buddy-update self
			     (next-toc-arg)
			     (string= "T" (next-toc-arg))
			     (parse-integer (next-toc-arg))
			     (parse-integer (next-toc-arg))
			     (parse-integer (next-toc-arg))
			     (next-toc-arg)
			     (next-toc-arg)))

       ;; UPDATED2:b:<username>:<unknown>:<alias>
       ;; We receive this when somebody's server-stored alias is
       ;; updated.
       ("UPDATED2"
	(handle-update self (next-toc-arg) (next-toc-arg) (next-toc-arg) (next-toc-arg)))

       ;; This packet describes a particular user's capabilities, such
       ;; as file transfer, buddy icons, etc.  As far as mapping
       ;; capability strings to capabilities goes, you're on your own.
       ("BUDDY_CAPS2"
	(handle-buddy-capabilities self
				   (next-toc-arg)
				   (let ((capabilities (parse-comma-delimited-list (rest-toc-string))))
				     ;; The buddy capabilities list
				     ;; always seems to end with a
				     ;; comma, e.g. "0,".
				     (if (= (length (car (last capabilities))) 0)
					 (butlast capabilities)
					 capabilities))))

       ;; BART2:<username>:<unknown>
       ;; The structure of this message is not yet understood. It most
       ;; likely provides buddy icon information about a user, such as
       ;; whether they have a buddy icon or not and the hashcode
       ;; necessary to request if from the server.
       ("BART2"
	(handle-bart self (next-toc-arg) (rest-toc-string)))

       ;; ERROR:<Error Code>:Var args
       ;; See the error-string function and *error-strings* list.
       ("ERROR"
	(let ((code (parse-integer (next-toc-arg)))
	      (args (rest-toc-args)))
	  (setf (aim-connection-last-error self) (cons code args))
	  (handle-toc-error self code args)))

       ;; EVILED:<new evil>:<name of eviler, blank if anonymous>
       ;; The user was just eviled/warned.
       ("EVILED"
	(handle-warned self (parse-integer (next-toc-arg)) (next-toc-arg)))
       
       ;; CHAT_JOIN:<Chat Room Id>:<Chat Room Name>
       ;; We were able to join this chat room.  The Chat Room Id is
       ;; internal to TOC.
       ("CHAT_JOIN"
	(handle-chat-join self (next-toc-arg) (next-toc-arg)))

       ;; CHAT_IN_ENC:<chatroom id>:<user>:<whisper T/F>:<???>:en:<message>
       ;; This command received instead of CHAT_IN. It is similar to
       ;; TOC 1.0 except there are a two new parameters.  One of them
       ;; is language; the other is unknown but is usually "A"
       ("CHAT_IN_ENC"
	(handle-chat-in self
			(make-instance 'chat-im
				       :chatroom-id (next-toc-arg)
				       :user (next-toc-arg)
				       :whisper-p (string= "T" (next-toc-arg))
				       :unk1 (next-toc-arg)
				       :encoding (next-toc-arg)
				       :text (rest-toc-string))))

       ;; CHAT_UPDATE_BUDDY:<Chat Room Id>:<Inside? T/F>:<User 1>:<User 2>...
       ;; This one command handles arrival/departs from a chat room.
       ;; The very first message of this type for each chat room
       ;; contains the users already in the room.
       ("CHAT_UPDATE_BUDDY"
	(handle-chat-update-buddy self (next-toc-arg) (string= "T" (next-toc-arg))
				  (rest-toc-args)))

       ;; CHAT_INVITE:<Chat Room Name>:<Chat Room Id>:<Invite Sender>:<Message>
       ;; We are being invited to a chat room.
       ("CHAT_INVITE"
	(handle-chat-invite self (next-toc-arg) (next-toc-arg) (next-toc-arg)
			    (rest-toc-string)))

       ;; CHAT_LEFT:<Chat Room Id>
       ;; Tells tic connection to chat room has been dropped.
       ("CHAT_LEFT"
	(handle-chat-left self (next-toc-arg)))

       ;; GOTO_URL:<Window Name>:<Url>
       ;; Goto a URL.  Window Name is the suggested internal name of
       ;; the window to use.  (Java supports this.)
       ("GOTO_URL"
	(handle-goto-url self (next-toc-arg) (rest-toc-string)))
       
       ;; PAUSE
       ;; Tells TIC to pause so we can do migration.  After receiving
       ;; the PAUSE message all messages sent to TOC will be ignored,
       ;; and in some cases the connection will be dropped.  Another
       ;; SIGN_ON message will be sent to let you know you are online
       ;; again. The buddy list and permit/deny items must be sent
       ;; again, followed by the toc_init_done.
       ("PAUSE"
	(handle-pause self))

       ;; CLIENT_EVENT2:<username>:<typing status>
       ;; These are typing notifications. 0 means stopped, 1 means
       ;; text entered, and 2 means typing.
       ("CLIENT_EVENT2"
	(handle-client-event self
			     (next-toc-arg)
			     (->typing-status (parse-integer (next-toc-arg)))))

       ;; INSERTED2
       ;; These will be sent whenever the buddy list is modified from
       ;; a different location, which happens when one is logged in in
       ;; two different places. It's a good idea to handle these,
       ;; otherwise the buddy list displayed could become out of synch
       ;; with what's on the server.
       ("INSERTED2"
	(let ((insertion-type (next-toc-arg)))
	  (stringcase insertion-type
	    ;; INSERTED2:g:<group name>
	    ;; A new group has been added to the buddy list.
	    ("g"
	     (handle-buddy-list-group-addition self (next-toc-arg)))

	    ;; INSERTED2:b:<alias>:<username>:<group>
	    ;; A new screenname has been added to the buddy list
 	    ("b"
	     (handle-buddy-list-buddy-addition self (next-toc-arg) (next-toc-arg) (next-toc-arg)))

	    ;; INSERTED2:d:<username>
	    ;; Somebody has been added to the deny list.
	    ("d"
	     (handle-deny-list-addition self (next-toc-arg)))

	    ;; INSERTED2:p:<username>
	    ;; Somebody has been added to the permit list.
	    ("p"
	     (handle-permit-list-addition self (next-toc-arg)))
	    (otherwise
	     (warn "Unknown INSERTED2 event type ~S in ~S" insertion-type string)))))

       ;; DELETED2
       ;; These commands are similar to the INSERTED2 commands, in
       ;; that they provide dynamic updates whenever the buddy list is
       ;; modified from a different location.
       ("DELETED2"
	(let ((insertion-type (next-toc-arg)))
	  (stringcase insertion-type
	    ;; DELETED2:g:<group name>
	    ;; A group has been deleted from the buddy list.
	    ("g"
	     (handle-buddy-list-group-deletion self (next-toc-arg)))

	    ;; DELETED2:b:<alias>:<username>:<group>
	    ;; A user has been removed from the buddy list.
 	    ("b"
	     (handle-buddy-list-buddy-deletion self (next-toc-arg) (next-toc-arg) (next-toc-arg)))

	    ;; DELETED2:d:<username>
	    ;; Somebody has been removed from the deny list.
	    ("d"
	     (handle-deny-list-deletion self (next-toc-arg)))

	    ;; DELETED2:p:<username>
	    ;; Somebody has been removed from the permit list.
	    ("p"
	     (handle-permit-list-deletion self (next-toc-arg)))
	    (otherwise
	     (warn "Unknown INSERTED2 event type ~S in ~S" insertion-type string)))))

       (otherwise
	(handle-unknown-event self string))))))

(defmethod handle-unknown-event ((self aim-connection) event-string)
  (warn "Unhandled AIM TOC command ~S" event-string))



;; ------------------------------------------------
;; Actions
;; ------------------------------------------------

(defmethod do-send-im ((self aim-connection) screen-name message &key (auto-p NIL))
  (send-toc self "toc2_send_im ~A ~A~A"
	    (normalize-string screen-name)
	    (encode-string message)
	    (if auto-p " auto" "")))

(defmethod do-add-buddies ((self aim-connection) buddies)
  (send-toc self "toc_add_buddy~{ ~A~}" (mapcar #'normalize-string buddies)))

(defmethod do-remove-buddies ((self aim-connection) buddies)
  (send-toc self "toc_remove_buddy~{ ~A~}" (mapcar #'normalize-string buddies)))

(defmethod do-add-permits ((self aim-connection) permits)
  (send-toc self "toc2_add_permit~{ ~A~}" (mapcar #'normalize-string permits)))

(defmethod do-remove-permits ((self aim-connection) permits)
  (send-toc self "toc2_remove_permit~{ ~A~}" (mapcar #'normalize-string permits)))

(defmethod do-add-denies ((self aim-connection) denies)
  (send-toc self "toc2_add_deny~{ ~A~}" (mapcar #'normalize-string denies)))

(defmethod do-remove-denies ((self aim-connection) denies)
  (send-toc self "toc2_remove_deny~{ ~A~}" (mapcar #'normalize-string denies)))


(deftype typing-status () '(member T NIL :paused))

(defparameter *typing-statuses*
  '((0 . NIL)
    (1 . :paused)
    (2 . T)))

(defun ->typing-status (code)
  (let ((pair (assoc code *typing-statuses*)))
    (if pair
	(cdr pair)
	:unknown)))

(defmethod do-send-typing-activity-event ((self aim-connection) user status)
  (check-type status typing-status)
  (let ((event-code (ecase status
		      ((NIL) 0)
		      ((:paused) 1)
		      ((T) 2))))
    (send-toc self "toc2_client_event ~A ~A" (normalize-string user) event-code)))
		      
(defmethod do-warn ((self aim-connection) screen-name anonymous-p)
  (send-toc self "toc_evil ~A ~A"
	    (normalize-string screen-name)
	    (if anonymous-p "anon" "norm")))

(defmethod do-set-idle-time ((self aim-connection) seconds)
  (send-toc self "toc_set_idle ~S" seconds))

(defmethod do-set-away-message ((self aim-connection) message)
  (if message
    (send-toc self "toc_set_away ~A" (encode-string message))
    (send-toc self "toc_set_away")))

(defmethod do-get-info ((self aim-connection) user)
  (send-toc self "toc_get_info ~A" (normalize-string user)))

(defmethod do-set-info ((self aim-connection) info)
  (send-toc self "toc_set_info ~A" (encode-string info)))

(defmethod do-chat-invite ((self aim-connection) room invitation buddies)
  (send-toc self "toc_chat_invite ~A ~A~{ ~A~}"
	    (normalize-string room)
	    (encode-string invitation)
	    (mapcar #'normalize-string buddies)))

(defmethod do-chat-accept ((self aim-connection) id)
  (send-toc self "toc_chat_accept ~A" id))

(defmethod do-chat-leave ((self aim-connection) id)
  (send-toc self "toc_chat_leave ~A" id))

(defmethod do-chat-whisper ((self aim-connection) room user message)
  (send-toc self "toc_chat_whisper ~A ~A ~A"
	    room (normalize-string user) (encode-string message)))

(defmethod do-chat-send ((self aim-connection) room message)
  (send-toc self "toc_chat_send ~A ~A"
	    room (encode-string message)))

(defmethod do-chat-join ((self aim-connection) room)
  ;; The docs says it's always 4...
  (let ((exchange 4))
    (send-toc self "toc_chat_join ~A ~A" exchange room)))

(defmethod do-set-config ((self aim-connection) config)
  (send-toc self "toc_set_config \"~A\"" config))

(defmethod do-get-dir ((self aim-connection) user)
  (send-toc self "toc_get_dir ~A" (encode-string user)))

(defmethod do-set-dir ((self aim-connection) info)
  (send-toc self "toc_set_dir ~A" info))

(defmethod do-dir-search ((self aim-connection) info)
  (send-toc self "toc_dir_search ~A" info))

;; Should only be called in response to receving FLAP signon from
;; server.
(defmethod send-flap-sign-on ((self aim-connection))
  (trace-log self "Sending FLAP signon.")
  (let ((normalized-name (normalize-string (aim-connection-username self))))
    (write-flap (aim-connection-stream self) :sign-on
		(flexi-streams:with-output-to-sequence (s)
		  (let ((s (flexi-streams:make-flexi-stream s :external-format *external-format*)))
		    (write-word2 0 s)
		    (write-byte2 0 s)
		    (write-byte2 1 s)
		    (write-word2 1 s)
		    (write-word2 (length normalized-name) s)
		    (write-string normalized-name s))))))

(defmethod send-toc-sign-on ((self aim-connection))
  (with-slots (login-host login-port username password language agent)
      self
    (send-toc self "toc2_login ~A ~A ~A ~A ~A ~A 160 US \"\" \"\" 3 0 30303 -kentucky -utf8 ~A"
	      login-host login-port (normalize-string username)
	      (roast-string password) language
	      (encode-string (format nil "TIC:~A" agent))
	      (magic-login-number (normalize-string username) password))))

;; Yeah.  It's magic.  See TOC2.txt.
(defun magic-login-number (username password)
  (flet ((char-to-byte (char)
	   (elt (flexi-streams:with-output-to-sequence (s)
		  (let ((s (flexi-streams:make-flexi-stream s :external-format *external-format*)))
		    (write-char char s)))
		0)))
    (let ((sn (- (char-to-byte (char username 0)) 96))
	  (pw (- (char-to-byte (char password 0)) 96)))
      (let ((a (+ (* sn 7696) 738816))
	    (b (* sn 746512)))
	(let ((c (* pw a)))
	  (+ c (- a) b 71665152))))))





(defmethod send-toc ((self aim-connection) fmt &rest args)
  (let ((msg (apply #'format nil fmt args)))
    (trace-log self "Sending TOC message ~S" msg)
    (write-toc (aim-connection-stream self) msg :type :data)))


;; --------------------
;; Reading and Writing TOC and FLAPs from and to streams.

(defun write-toc (stream string &key (type :data))
  (let ((payload (flexi-streams:with-output-to-sequence (s)
		   (let ((s (flexi-streams:make-flexi-stream s :external-format *external-format*)))
		     (write-string string s)
		     (write-byte 0 s)))))
    (write-flap stream type payload)))


(defvar *debug-flap* NIL)

(defun read-flap (stream)
  ;; Assume any errors here mean we were disconnected.  A somewhat
  ;; dangerous assumption, really.
  (handler-case
   (progn
     (assert (eql (read-char stream) #\*))
     (let* ((frame-type (%read-byte stream))
	    (sequence-number (read-word stream))
	    (data-length (read-word stream)))
       (let ((s (with-output-to-string (s)
				       (dotimes (i data-length)
					 (write-char (read-char stream) s)))))
	 (when *debug-flap*
	   (format *debug-io* "~&<-- * [~S] ~s byte frame, type ~S: ~S"
		   sequence-number data-length (code-frame-type frame-type) s))
	 (values s (code-frame-type frame-type) sequence-number))))
   (error (e)
     (error 'disconnect-error :original-error e))))

(defun write-flap (stream type payload)
  (let ((len (length payload)))
    (let ((sequence-number (next-sequence-number)))
      (when *debug-flap*
	(format *debug-io* "~&--> * [~S] ~S byte frame, type ~S: ~S"
		sequence-number len type payload))
      ;; Assume any errors here mean we were disconnected.  A somewhat
      ;; dangerous assumption, really.
      (handler-case
	  (progn
	    (write-char #\* stream)
	    (write-byte2 (frame-type-code type) stream)
	    (write-word2 sequence-number stream)
	    (write-word2 len stream)
	    (write-sequence payload stream)
	    (finish-output stream))
	(error (e)
	  (error 'disconnect-error :original-error e))))))

(defvar *sequence-number* (random 65536))
(defun next-sequence-number ()
  (mod (incf *sequence-number*) 65536))


(define-condition disconnect-error (error)
  ((original-error :reader original-error :initarg :original-error))
  (:report (lambda (condition stream)
	     (format stream "~A" (original-error condition)))))

(defun code-frame-type (code)
  (ecase code
    (1 :sign-on)
    (2 :data)
    (3 :error)
    (4 :sign-off)
    (5 :keep-alive)))

(defun frame-type-code (type)
  (ecase type
    (:sign-on    1)
    (:data       2)
    (:error      3)
    (:sign-off   4)
    (:keep-alive 5)))

(defun write-word2 (integer stream)
  (write-byte2 (ash integer -8) stream)
  (write-byte2 (logand integer 255) stream))

(defun write-byte2 (integer stream)
  (write-byte integer stream))

(defun read-word (stream &optional (eof-error-p T) eof-value)
  (let ((byte1 (%read-byte stream eof-error-p :eof)))
    (if (eq byte1 :eof)
      eof-value
      (let ((byte2 (%read-byte stream eof-error-p :eof)))
	(if (eq byte2 :eof)
	  eof-value
	  (+ (* byte1 256) byte2))))))

(defun %read-byte (stream &optional (eof-error-p T) eof-value)
  (read-byte stream eof-error-p eof-value))
    
(defun %send-string (stream string)
  (write-string string stream)
  (finish-output stream))



;; --------------------
;; String utilities

(defun encode-string (string)
  (with-output-to-string (s)
    (write-char #\" s)
    (dotimes (i (length string))
      (let ((char (char string i)))
	(when (member char '(#\$ #\{ #\} #\[ #\] #\( #\) #\" #\\))
	  (write-char #\\ s))
	(write-char char s)))
    (write-char #\" s)))



(defun string-to-bytes (string)
  (flexi-streams:with-output-to-sequence (stream)
    (let ((s (flexi-streams:make-flexi-stream stream :external-format *external-format*)))
      (write-string string s))))

(defvar *roast-string* "Tic/Toc")

(defun roast-string (string &optional (roaster *roast-string*))
  (let ((roaster-bytes (string-to-bytes roaster))
	(string-bytes (string-to-bytes string)))
  (with-output-to-string (s)
    (let ((rlen (length roaster-bytes)))
      (write-string "0x" s)
      (dotimes (i (length string))
	(format s "~2,'0X"
		(logxor (elt string-bytes i)
			(elt roaster-bytes (mod i rlen)))))))))

(defun normalize-string (string)
  (remove #\space (string-downcase string)))


(defun next-delimited-token (message delimiter-char &key (start 0))
  (let ((delim-position (position delimiter-char message :start start :test #'char=)))
    (if delim-position
	(values (subseq message start delim-position)
		(if (> (length message) delim-position)
		    (+ delim-position 1)
		    nil))
	(values (subseq message start) nil))))


(defun parse-comma-delimited-list (string)
  (let ((items '()))
    (labels ((collect (position)
	       (when position
		 (multiple-value-bind (item next-position)
		     (next-delimited-token string #\, :start position)
		   (when item
		     (push item items)
		     (collect next-position))))))
      (collect 0)
      (nreverse items))))


(provide :claim)


#|

;; AOL enforces a limit on the rate at which messages can be sent;
;; it's easy for a bot that talks to multiple people simultaneously to
;; go over the "server speed limit".

(defclass throttled-aim-connection (aim-connection)
  ((max-send-rate :accessor aim-connection-max-send-rate :initform nil
		  :initarg :max-send-rate)
   (regulator-process :accessor regulator-process :initform nil)
   (toc-queue :accessor toc-queue :initform (make-instance 'util:synched-queue))))

    
(defmethod send-toc ((self throttled-aim-connection) fmt &rest args)
  (let ((msg (apply #'format nil fmt args)))
    (util:q-add (toc-queue self) msg)))


(defmethod toc-queue-regulator ((self throttled-aim-connection))
  (flet ((wait-for-toc-in-queue (queue)
	   (process-wait "Waiting for queue"
			 #'(lambda ()
			     (not (util:q-empty-p queue))))))
    (loop
      (wait-for-toc-in-queue (toc-queue self))
      (write-toc (aim-connection-stream self)
		 (util:q-head (toc-queue self)))
      (sleep (/ 1.0 (aim-connection-max-send-rate self))))))


(defmethod close-aim-connection :around ((self throttled-aim-connection))
  (call-next-method)
  (when (regulator-process self)
    (process-kill (regulator-process self))))

(defmethod open-aim-connection :after ((self throttled-aim-connection))
  (setf (regulator-process self)
	(process-run-function "AIM throttler"
			      #'toc-queue-regulator
			      self)))
|#

#|

;; One way to run the AIM connection in a different process.

(defclass async-aim-connection (aim-connection)
  ((handler-proc :accessor aim-connection-handler-proc :initform nil)))

(defmethod open-aim-connection :after ((self async-aim-connection))
  (setf (aim-connection-handler-proc self) (make-aim-handler-process self)))
  
(defmethod close-aim-connection :before ((self async-aim-connection))
  (when (aim-connection-handler-proc self)
    (process-kill (aim-connection-handler-proc self))
    (setf (aim-connection-handler-proc self) nil)))

(defvar *process-number* 0)

(defmethod make-aim-handler-process ((self async-aim-connection))
  (process-run-function (format nil "AIM Handler ~S" (incf *process-number*))
			'handler-process-loop self))

(defmethod handler-process-loop ((self async-aim-connection))
  (if (not (aim-connection-auto-reconnect-p self))
    (receive-events self)
    (let ((continue-p T))
      (do ()
	  ((not continue-p))
	(handler-case
	 (receive-events self)
	 (disconnect-error (e)
	   (warn "AIM connection got the following error: ~A" e)
	   (setf continue-p (handle-disconnection self)))))
      (close (aim-connection-stream self))
      (setf (aim-connection-stream self) nil))))

|#



