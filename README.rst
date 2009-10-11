CLAIM - Common Lisp AIM Interface
=================================

Copyright 2001, 2009 John Wiseman

Version 1.1

Licensed under the MIT license--see the accompanying LICENSE.txt
file.


INTRODUCTION
------------

CLAIM is an implementation of AOL's semi-open Instant Messaging
protocol, TOC (see PROTOCOL.txt).  It allows one to build chatbots and
other AIM clients.

QUICKSTART
----------

Load this code (with ASDF, ``(asdf:operate 'asdf:load-op :claim)``
should be sufficient once you've put the directory containing the
claim.asd file in ``asdf:*central-registry*``), then load the
``examples/gossip-bot.lisp`` file.  At the listener, run the
start-gossip-bot function::

  ? (start-gossip-bot "myusername" "mypassword")

You must use the username and password of an existing AIM account (see
http://aim.aol.com/ on how to create a free AIM account).

Anyone can now send messages to and interact with the gossip bot.

REQUIREMENTS & DEPENDENCIES
---------------------------

CLAIM requires minimal TCP socket support.  The file sysdeps.lisp
contains a single function definition, ``MAKE-TCP-SOCKET``, that opens a
TCP socket.  Implementations are provided for OpenMCL, ACL, LispWorks
and SBCL.

IMPLEMENTATION NOTES
--------------------

The TOC protcol is slightly binary, mostly text.  Accordingly, I open
socket streams in text mode and fake the binary parts.  It won't work
unless your Lisp's code-char and char-code functions use ASCII or some
ASCII supersets.

I've written the code without assuming multiprocessing, but in a way
that should be easily integrated with most multiprocessing
implementations.  Let me know if it doesn't work out that way.

I checked all the other AIM libraries I could find, and nobody even
tries to deal with the "server speed limit", which was a constant
problem for me.  See the comments at the end of this file for one
attempt to solve this problem.

The following implementations of TOC were helpful when writing this
one:

* http://sourceforge.net/projects/tnt/
* http://www.wiredfool.com/ftoc/ (Eric Soroos)

This code has been tested in ACL, OpenMCL, SBCL and LispWorks.

BUGS
----

There should be much more documentation.  I've tried to structure the
code so that it is easy to extend and use for chat clients, but
documenting what I did would help a lot too.  Sorry.

The gossip bot should do some HTML parsing if it wants to work
perfectly with iChat; When the bot sends "Someone said you smell",
iChat shows "you smell".
