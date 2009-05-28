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
;;; ASDF system definition.

(in-package #:asdf)

(defsystem #:claim
  :name "CLAIM"
  :author "John Wiseman <jjwiseman@yahoo.com>"
  :version "1.2"
  :maintainer "John Wiseman <jjwiseman@yahoo.com>"
  :licence "MIT"
  :description "Common Lisp AOL Instant Messenger (AIM) client interface, or CLAIM"
  :long-description "CLAIM provides a client interface to AOL's Instant Messaging network using the TOC protocol.  CLAIM supports ACL, LispWorks, OpenMCL and SBCL."

  :components ((:file "package")
	       (:file "sysdeps" :depends-on ("package"))
	       (:file "claim" :depends-on ("package" "sysdeps")))
  :depends-on (#:flexi-streams #+sbcl #:sb-bsd-sockets))
