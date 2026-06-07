(in-package :sb-manual)

(defsection @networking (:title "Networking")
  "The `SB-BSD-SOCKETS` module provides a thinly disguised BSD
  socket API for SBCL. Ideas have been stolen from the BSD socket API
  for C and Graham Barr's `IO::Socket` classes for Perl.

  Sockets are represented as CLOS objects, and the API naming
  conventions attempt to balance between the BSD names and good lisp
  style."
  (@sockets-overview section)
  (@general-sockets section)
  (@socket-options section)
  (@inet-domain-sockets section)
  (@local-domain-sockets section)
  (@name-service section))

(defsection @sockets-overview (:title "Sockets Overview")
  "Most of the functions are modelled on the BSD socket API. BSD sockets
  are widely supported, portably (by Unix standards, at least)
  available on a variety of systems, and documented. There are some
  differences in approach where we have taken advantage of some of the
  more useful features of Common Lisp -- briefly:

  - Where the C API would typically return -1 and set `errno`,
    `SB-BSD-SOCKETS` signals an error. All the errors are subclasses
    of SB-BSD-SOCKETS:SOCKET-ERROR and generally correspond one for
    one with possible `errno` values.

  - We use multiple return values in many places where the C API would
    use pass-by-reference values.

  - We can often avoid supplying an explicit length argument to
    functions because we already know how long the argument is.

  - IP addresses and ports are represented in slightly friendlier
    fashion than \"network-endian integers\".")

(defsection @general-sockets (:title "General Sockets")
  (sb-bsd-sockets:socket class)
  (sb-bsd-sockets:socket-bind function)
  (sb-bsd-sockets:socket-accept function)
  (sb-bsd-sockets:socket-connect function)
  (sb-bsd-sockets:socket-peername function)
  (sb-bsd-sockets:socket-name function)
  (sb-bsd-sockets:socket-receive function)
  (sb-bsd-sockets:socket-send function)
  (sb-bsd-sockets:socket-listen function)
  (sb-bsd-sockets:socket-open-p function)
  (sb-bsd-sockets:socket-close function)
  (sb-bsd-sockets:socket-shutdown function)
  (sb-bsd-sockets:socket-make-stream function)
  (sb-bsd-sockets:socket-error function)
  (sb-bsd-sockets:non-blocking-mode function))

(defsection @socket-options (:title "Socket Options")
  "A subset of socket options are supported, using a fairly general
  framework which should make it simple to add more as required -- see
  `\\\\SYS:CONTRIB;SB-BSD-SOCKETS:SOCKOPT.LISP` for details. The name
  mapping from C is fairly straightforward: `\\\\SO_RCVLOWAT` becomes
  SB-BSD-SOCKETS:SOCKOPT-RECEIVE-LOW-WATER and `(SETF
  SB-BSD-SOCKETS:SOCKOPT-RECEIVE-LOW-WATER)`."
  (sb-bsd-sockets:sockopt-reuse-address function)
  (sb-bsd-sockets:sockopt-keep-alive function)
  (sb-bsd-sockets:sockopt-oob-inline function)
  (sb-bsd-sockets:sockopt-bsd-compatible function)
  (sb-bsd-sockets:sockopt-pass-credentials function)
  (sb-bsd-sockets:sockopt-debug function)
  (sb-bsd-sockets:sockopt-dont-route function)
  (sb-bsd-sockets:sockopt-broadcast function)
  (sb-bsd-sockets:sockopt-tcp-nodelay function))

(defsection @inet-domain-sockets (:title "INET Domain Sockets")
  "The TCP and UDP sockets that you know and love. Some representation
  issues:

  - IPv4 Internet addresses are represented by vectors of
    `(UNSIGNED-BYTE 8)` (e.g. `#(127 0 0 1)`). Ports are just
    integers. No conversion between network- and host-order data is
    needed from the user of this package.

  - IPv6 Internet addresses are represented by length 16 vectors of
    `(UNSIGNED-BYTE 8)` (e.g. `#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)`.
    Ports are just integers. As for IPv4 addresses, no conversion
    between network- and host-order data is needed from the user of
    this package.

  - Socket addresses are represented by the two values for address and
    port, so for example, `(SB-BSD-SOCKETS:SOCKET-CONNECT SOCKET #(192
    168 1 1) 80)` for IPv4 and `(SB-BSD-SOCKETS:SOCKET-CONNECT SOCKET
    #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 80)` for IPv6."
  (sb-bsd-sockets:inet-socket class)
  (sb-bsd-sockets:inet6-socket class)
  (sb-bsd-sockets:make-inet-address function)
  (sb-bsd-sockets:make-inet6-address function)
  (sb-bsd-sockets:get-protocol-by-name function))

(defsection @local-domain-sockets (:title "Local Domain Sockets")
  "Local domain (`\\\\AF_LOCAL`) sockets are also known as Unix-domain
  sockets but were renamed by POSIX presumably on the basis that they
  may be available on other systems too.

  A local socket address is a string, which is used to create a node
  in the local filesystem. This means of course that they cannot be
  used across a network."
  (sb-bsd-sockets:local-socket class)
  "A local abstract socket address is also a string the scope of which is
  the local machine. However, in contrast to a local socket address, there
  is no corresponding filesystem node."
  (sb-bsd-sockets:local-abstract-socket class))

(defsection @name-service (:title "Name Service")
  "Presently name service is implemented by calling out to the
  `getaddrinfo(3)` and `gethostinfo(3)`, or to `gethostbyname(3)` and
  `gethostbyaddr(3)` on platforms where the preferred functions are
  not available. The exact details of the name resolving process (for
  example the choice of whether DNS or a hosts file is used for
  lookup) are platform dependent."
  ;; Direct links to the asynchronous `resolver(3)` routines would be
  ;; nice to have eventually, so that we can do DNS lookups in
  ;; parallel with other things.
  (sb-bsd-sockets:host-ent class)
  (sb-bsd-sockets:get-host-by-name function)
  (sb-bsd-sockets:get-host-by-address function)
  (sb-bsd-sockets:host-ent-address function))
