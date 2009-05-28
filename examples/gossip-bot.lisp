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
;;; A gossip bot for AIM, using CLAIM.

(defpackage :gossip-bot
  (:use :common-lisp
	:com.lemonodor.claim)
  (:export #:start-gossip-bot))

(in-package :gossip-bot)

(defun start-gossip-bot (username password)
  (let ((bot (make-instance 'gossip-bot
			    :username username
			    :password password)))
    (open-aim-connection bot)
    (unwind-protect
	(receive-events bot)
      (close-aim-connection bot))))
			 

(defclass gossip-bot (aim-connection)
  ((messages :accessor messages :initform '())
   (users :accessor users :initform nil)))

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defmethod handle-im-in ((self gossip-bot) im)
  (let ((user (im-user im))
	(message (im-text im)))
    (format T "~&~A: ~A" user message)
    (if (not (member user (users self) :test #'equalp))
	(do-send-im self user
		    (format nil "Hi, ~A.  I'm a gossip bot.  Tell me some stuff." user))
	(progn
	  (push (cons user message) (messages self))
	  (if (zerop (random 4))
	      (do-send-im self user
			  (format nil "Someone, maybe ~A, said ~A"
				  (random-elt (users self))
				  (cdr (random-elt (messages self)))))
	      (do-send-im self user
			  (format nil "Someone said ~A" (cdr (random-elt (messages self))))))))
    (pushnew user (users self) :test #'equal)))

(defmethod do-send-im :around ((self gossip-bot) user message &key (auto-p))
  (declare (ignore user auto-p))
  (format T "~&~A: ~A" (aim-connection-username self) message)
  (call-next-method))
   
(defmethod handle-warned ((self gossip-bot) warn-level warner)
  (declare (ignore warn-level))
  (do-send-im self warner "I bet that made you feel tough.")
  (if (zerop (random 2))
      (do-warn self warner NIL)))
