;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; CLAIM -- An implementation of the AIM TOC protocol in Lisp.
;;;
;;; Copyright 2001, 2002, 2004 I/NET Inc. (http://www.inetmi.com/)
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2004-10-29
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; Lisp-specific functions.

(in-package :common-lisp)


#+(and mcl (not openmcl))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :opentransport))


(defun com.lemonodor.claim::make-tcp-socket (host port)
  #+openmcl
  (ccl:make-socket :connect :active
		   :remote-host host
		   :remote-port port)

  #+(and mcl (not openmcl))
  (ccl::open-tcp-stream host port :element-type '(unsigned-byte 8))

  #+allegro
  (socket:make-socket :connect :active
		      :remote-host host
		      :remote-port port)

  #+lispworks
  (comm:open-tcp-stream host port)

  #+sbcl
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
		  :type :stream
		  :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket
				   (car (sb-bsd-sockets:host-ent-addresses
					 (sb-bsd-sockets:get-host-by-name host)))
				   port)
    (sb-bsd-sockets:socket-make-stream socket
				       :element-type '(unsigned-byte 8)
				       :input T
				       :output T
				       :buffering :none))
  )