(cl:in-package #:sb-bsd-sockets)

;;; Addresses

(defgeneric make-sockaddr-for (socket &optional sockaddr &rest address)
  (:documentation
   "Return a Socket Address object suitable for use with SOCKET.
When SOCKADDR is passed, it is used instead of a new object."))

(defgeneric free-sockaddr-for (socket sockaddr)
  (:documentation
   "Deallocate the socket address object SOCKADDR that was created for
SOCKET."))

(defgeneric bits-of-sockaddr (socket sockaddr &optional size)
  (:documentation
   "Return as multiple values protocol-dependent bits of parameter
SOCKADDR, e.g. the host/port if SOCKET is an inet socket."))

(defgeneric size-of-sockaddr (socket)
  (:documentation
   "Return the size in bytes of a sockaddr object for SOCKET."))

;;; Sockets

(defgeneric socket-name (socket)
  (:documentation
   "Return the address (as vector of bytes) and port that SOCKET is
bound to, as multiple values."))

(defgeneric socket-peername (socket)
  (:documentation
   "Return SOCKET's peer; depending on the address family this may
return multiple values"))

(defgeneric socket-namestring (socket)
  (:method ((socket t))
    nil)
  (:documentation
   "Return a string representation of the address and port that SOCKET
is bound to."))

(defgeneric socket-peerstring (socket)
  (:method ((socket t))
    nil)
  (:documentation
   "Return a string representation of the address and port of SOCKET's
peer."))

(defgeneric socket-open-p (socket)
  (:documentation
   "Return true if SOCKET is open; otherwise, return false.")
  (:method ((socket t))
    (error 'type-error :datum socket :expected-type 'socket)))

(defgeneric socket-close (socket &key abort)
  (:documentation
   "Close SOCKET, unless it was already closed.

If SOCKET-MAKE-STREAM has been called, calls CLOSE using ABORT on that
stream.  Otherwise closes the socket file descriptor using
close(2)."))

(defgeneric socket-bind (socket &rest address)
  (:documentation
   "Bind SOCKET to ADDRESS, which may vary according to socket family.
For the INET family, pass ADDRESS and PORT as two arguments; for FILE
address family sockets, pass the filename string.  See also bind(2)"))

(defgeneric socket-accept (socket)
  (:documentation
   "Perform the accept(2) call, returning a newly-created connected
socket and the peer address as multiple values"))

(defgeneric socket-connect (socket &rest address)
  (:documentation
   "Perform the connect(2) call to connect SOCKET to a remote PEER.
No useful return value."))

(defgeneric socket-receive (socket buffer length
                            &key
                            oob peek waitall dontwait element-type)
  (:documentation
   "Read LENGTH octets from SOCKET into BUFFER (or a freshly-consed
buffer if NIL), using recvfrom(2). If LENGTH is NIL, the length of
BUFFER is used, so at least one of these two arguments must be
non-NIL. If BUFFER is supplied, it had better be of an element type
one octet wide. Returns the buffer, its length, and the address of the
peer that sent it, as multiple values. On datagram sockets, sets
MSG_TRUNC so that the actual packet length is returned even if the
buffer was too small."))

(defgeneric socket-send (socket buffer length
                         &key
                         address
                         external-format
                         oob eor dontroute dontwait nosignal
                         #+linux confirm #+linux more)
  (:documentation
   "Send LENGTH octets from BUFFER into SOCKET, using sendto(2). If
BUFFER is a string, it will converted to octets according to
EXTERNAL-FORMAT. If LENGTH is NIL, the length of the octet buffer is
used. The format of ADDRESS depends on the socket type (for example
for INET domain sockets it would be a list of an IP address and a
port). If no socket address is provided, send(2) will be called
instead. Returns the number of octets written."))

(defgeneric socket-listen (socket backlog)
  (:documentation
   "Mark SOCKET as willing to accept incoming connections.  The
integer BACKLOG defines the maximum length that the queue of pending
connections may grow to before new connection attempts are refused.
See also listen(2)"))

(defgeneric socket-shutdown (socket &key direction)
  (:documentation
   "Indicate that no communication in DIRECTION will be performed on
SOCKET.

DIRECTION has to be one of :INPUT, :OUTPUT or :IO.

After a shutdown, no input and/or output of the indicated DIRECTION
can be performed on SOCKET."))

(defgeneric socket-make-stream (socket &key input output
                                            element-type external-format
                                            buffering
                                            timeout)
  (:documentation
   "Find or create a STREAM that can be used for IO on SOCKET \(which
must be connected\).  Specify whether the stream is for INPUT, OUTPUT,
or both \(it is an error to specify neither\).

ELEMENT-TYPE and EXTERNAL-FORMAT are as per OPEN.

TIMEOUT specifies a read timeout for the stream."))

(defgeneric non-blocking-mode (socket)
  (:documentation
   "Is SOCKET in non-blocking mode?"))

(defgeneric (setf non-blocking-mode) (non-blocking-p socket)
  (:documentation
   "Put SOCKET in non-blocking mode - or not, according to
NON-BLOCKING-P"))

;;; Name service

(defgeneric host-ent-address (host-ent)
  (:documentation
   "Return some valid address for HOST-ENT."))
