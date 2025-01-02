(defpackage "SB-BSD-SOCKETS-TEST"
  (:import-from #:test-util #:deftest)
  (:use "CL" "SB-BSD-SOCKETS"))

(in-package :sb-bsd-sockets-test)

;;; a real address
(deftest make-inet-address
  (equalp (make-inet-address "127.0.0.1")  #(127 0 0 1))
  t)
;;; and an address with bit 8 set on some octets
(deftest make-inet-address2
  (equalp (make-inet-address "242.1.211.3")  #(242 1 211 3))
  t)

(deftest make-inet6-address.1
    (equalp (make-inet6-address "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")
            #(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255))
  t)

(deftest unparse-inet6-address
    (string= (sb-bsd-sockets::unparse-inet6-address
              (make-inet6-address "fe80::abcd:1234"))
             "fe80::abcd:1234")
  t)

(deftest get-protocol-by-name/tcp
    (integerp (get-protocol-by-name "tcp"))
  t)

(deftest get-protocol-by-name/udp
  (integerp (get-protocol-by-name "udp"))
  t)

;;; See https://bugs.launchpad.net/sbcl/+bug/659857
;;; Apparently getprotobyname_r on FreeBSD says -1 and EINTR
;;; for unknown protocols...
#-(and freebsd sb-thread)
#-(and dragonfly sb-thread)
(deftest get-protocol-by-name/error
  (handler-case (get-protocol-by-name "nonexistent-protocol")
    (unknown-protocol ()
      t)
    (:no-error ()
      nil))
  t)

(eval-when (:compile-toplevel :execute)
  (when (handler-case (make-instance 'inet-socket
                                     :type :stream
                                     :protocol (get-protocol-by-name "tcp"))
          (error nil)
          (:no-error (x) x))
    (format t "~&Will test IPv4~%")
    (push :ipv4-support *features*)))

#+ipv4-support
(deftest make-inet-socket.smoke
  ;; make a socket
  (let ((s (make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "tcp"))))
    (> (socket-file-descriptor s) 1))
  t)

#+ipv4-support
(deftest make-inet-socket.keyword
    ;; make a socket
    (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
      (> (socket-file-descriptor s) 1))
  t)

#+ipv4-support
(deftest make-inet-socket-wrong
    ;; fail to make a socket: check correct error return.  There's no nice
    ;; way to check the condition stuff on its own, which is a shame
    (handler-case
        (make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "udp"))
      ;; CLH FIXME! some versions of darwin just return a socket error
      ;; here, not socket-type-not-supported-error or
      ;; protocol-not-supported-error.
      ((or #+darwin socket-error
        #+haiku address-family-not-supported
        operation-not-supported-error
        socket-type-not-supported-error
        protocol-not-supported-error)
          ()
        t)
      (:no-error nil))
  t)

#+ipv4-support
(deftest make-inet-socket-keyword-wrong
    ;; same again with keywords
    (handler-case
        (make-instance 'inet-socket :type :stream :protocol :udp)
      ;; CLH FIXME! some versions of darwin just return a socket error
      ;; here, not socket-type-not-supported-error or
      ;; protocol-not-supported-error.
      ((or
        #+darwin socket-error
        #+haiku address-family-not-supported
        operation-not-supported-error
        protocol-not-supported-error
        socket-type-not-supported-error)
          ()
        t)
      (:no-error nil))
  t)

(deftest make-inet6-socket.smoke
  (handler-case
      (let ((s (make-instance 'inet6-socket :type :stream :protocol (get-protocol-by-name "tcp"))))
        (> (socket-file-descriptor s) 1))
    ((or address-family-not-supported protocol-not-supported-error) () t))
  t)

(deftest make-inet6-socket.keyword
  (handler-case
      (let ((s (make-instance 'inet6-socket :type :stream :protocol :tcp)))
        (> (socket-file-descriptor s) 1))
    ((or address-family-not-supported protocol-not-supported-error) () t))
  t)

#+ipv4-support
(deftest non-block-socket
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (non-blocking-mode s) t)
    (non-blocking-mode s))
  t)

#+ipv4-support
(test-util:with-test (:name :inet-socket-bind)
  (let* ((tcp (get-protocol-by-name "tcp"))
         (address (make-inet-address "127.0.0.1"))
         (s1 (make-instance 'inet-socket :type :stream :protocol tcp))
         (s2 (make-instance 'inet-socket :type :stream :protocol tcp))
         (failure)
         (got-addrinuse))
    (format t "~&::: INFO: made sockets~%")
    (unwind-protect
         ;; Given the functions we've got so far, if you can think of a
         ;; better way to make sure the bind succeeded than trying it
         ;; twice, let me know
         (progn
           (socket-bind s1 address 0)
           (handler-case
               (let ((port (nth-value 1 (socket-name s1))))
                 (socket-bind s2 address port) ; should fail
                 nil)
             (address-in-use-error () (setq got-addrinuse t))
             (condition (c) (setq failure c))))
      (socket-close s1)
      (socket-close s2))
    (cond (failure (error "BIND failed with ~A" failure))
          ((not got-addrinuse) (error "Expected ADDRESS-IN-USE err")))))

(test-util:with-test (:name :inet6-socket-bind)
  (let ((notsupp)
        (failure)
        (got-addrinuse))
  (handler-case
      (let* ((tcp (get-protocol-by-name "tcp"))
             (address (make-inet6-address "::1"))
             (s1 (make-instance 'inet6-socket :type :stream :protocol tcp))
             (s2 (make-instance 'inet6-socket :type :stream :protocol tcp)))
        (format t "~&::: INFO: made sockets~%")
        (unwind-protect
             ;; Given the functions we've got so far, if you can think of a
             ;; better way to make sure the bind succeeded than trying it
             ;; twice, let me know
             (handler-case
                 (socket-bind s1 address 0)
               (socket-error ()
                 ;; This may mean no IPv6 support, can't fail a test
                 ;; because of that (address-family-not-supported doesn't catch that)
                 (setf notsupp t))
               (:no-error (x)
                 (declare (ignore x))
                 (handler-case
                     (let ((port (nth-value 1 (socket-name s1))))
                       (socket-bind s2 address port) ; should fail
                       nil)
                   (address-in-use-error () (setq got-addrinuse t))
                   (condition (c) (setq failure c)))))
          (socket-close s1)
          (socket-close s2)))
    ((or address-family-not-supported protocol-not-supported-error) ()
      (setq notsupp t)))
  (cond (notsupp (format t "~&INFO: not supported~%"))
        (failure (error "BIND failed with ~A" failure))
        ((not got-addrinuse) (error "Expected ADDRESS-IN-USE err")))))

#+ipv4-support
(deftest simple-sockopt-test
  ;; test we can set SO_REUSEADDR on a socket and retrieve it, and in
  ;; the process that all the weird macros in sockopt happened right.
  (let ((s (make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "tcp"))))
    (setf (sockopt-reuse-address s) t)
    (sockopt-reuse-address s))
  t)

(defun read-buf-nonblock (buffer stream)
  "Like READ-SEQUENCE, but returns early if the full quantity of data isn't there to be read.  Blocks if no input at all"
  (let ((eof (gensym)))
    (do ((i 0 (1+ i))
         (c (read-char stream nil eof)
            (read-char-no-hang stream nil eof)))
        ((or (>= i (length buffer)) (not c) (eq c eof)) i)
      (setf (elt buffer i) c))))

#+internet-available
(deftest name-service-return-type
  (vectorp (host-ent-address (get-host-by-address #(127 0 0 1))))
  t)

;;; these require that the echo services are turned on in inetd
#+internet-available
(deftest simple-tcp-client
    (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp))
          (data (make-string 200)))
      (socket-connect s #(127 0 0 1) 7)
      (let ((stream (socket-make-stream s :input t :output t :buffering :none)))
        (format stream "here is some text")
        (let ((data (subseq data 0 (read-buf-nonblock data stream))))
          (format t "~&Got ~S back from TCP echo server~%" data)
          (> (length data) 0))))
  t)

#+internet-available
(deftest sockaddr-return-type
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (socket-connect s #(127 0 0 1) 7)
           (multiple-value-bind (host port) (socket-peername s)
             (and (vectorp host)
                  (numberp port))))
      (socket-close s)))
  t)

#+internet-available
(deftest simple-udp-client
  (let ((s (make-instance 'inet-socket :type :datagram :protocol (get-protocol-by-name "udp")))
        (data (make-string 200)))
    (format t "Socket type is ~A~%" (sockopt-type s))
    (socket-connect s #(127 0 0 1) 7)
    (let ((stream (socket-make-stream s :input t :output t :buffering :none)))
      (format stream "here is some text")
      (let ((data (subseq data 0 (read-buf-nonblock data stream))))
        (format t "~&Got ~S back from UDP echo server~%" data)
        (> (length data) 0))))
  t)

;;; A fairly rudimentary test that connects to the syslog socket and
;;; sends a message.  Priority 7 is kern.debug; you'll probably want
;;; to look at /etc/syslog.conf or local equivalent to find out where
;;; the message ended up

#-win32
(deftest simple-local-client
    (progn
      ;; SunOS (Solaris) and Darwin systems don't have a socket at
      ;; /dev/log.  We might also be building in a chroot or
      ;; something, so don't fail this test just because the file is
      ;; unavailable, or if it's a symlink to some weird character
      ;; device.
      (when (block nil
              (handler-bind ((sb-posix:syscall-error
                              (lambda (e)
                                (declare (ignore e))
                                (return nil))))
                (sb-posix:s-issock
                 (sb-posix::stat-mode (sb-posix:stat "/dev/log")))))
        (let ((s (make-instance 'local-socket :type :datagram)))
          (format t "Connecting ~A... " s)
          (finish-output)
          (handler-case
              (socket-connect s "/dev/log")
            (sb-bsd-sockets::socket-error ()
              (setq s (make-instance 'local-socket :type :stream))
              (format t "failed~%Retrying with ~A... " s)
              (finish-output)
              (socket-connect s "/dev/log")))
          (format t "ok.~%")
          (let ((stream (socket-make-stream s :input t :output t :buffering :none)))
            (format stream
                    "<7>bsd-sockets: Don't panic.  We're testing local-domain client code; this message can safely be ignored"))))
      t)
  t)

;;; these require that the internet (or bits of it, at least) is available

#+internet-available
(deftest get-host-by-name.v4
  (equalp (car (host-ent-addresses (get-host-by-name "a.root-servers.net")))
          #(198 41 0 4))
  t)

#+internet-available
(deftest get-host-by-name.v6
  (equalp (car (host-ent-addresses (nth-value 1 (get-host-by-name "a.root-servers.net"))))
          #(32 1 5 3 186 62 0 0 0 0 0 0 0 2 0 48))
  t)

#+internet-available
(deftest get-host-by-address.v4
    (host-ent-name (get-host-by-address #(198 41 0 4)))
  "a.root-servers.net")

#+internet-available
(deftest get-host-by-address.v6
    (host-ent-name (get-host-by-address #(32 1 5 3 186 62 0 0 0 0 0 0 0 2 0 48)))
  "a.root-servers.net")

;;; These days lots of people seem to be using DNS servers that don't
;;; report resolving failures for non-existing domains. This test
;;; will fail there, so we've disabled it.
#+nil
(deftest get-host-by-name-wrong
  (handler-case
   (get-host-by-name "foo.tninkpad.telent.net.")
   (NAME-SERVICE-ERROR () t)
   (:no-error nil))
  t)

(defun http-stream (host port request)
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-connect
     s (car (host-ent-addresses (get-host-by-name host))) port)
    (let ((stream (socket-make-stream s :input t :output t :buffering :none)))
      (format stream "~A HTTP/1.0~%~%" request))
    s))

#+internet-available
(deftest simple-http-client-1
    (handler-case
        (let ((s (http-stream "ww.telent.net" 80 "HEAD /")))
          (let ((data (make-string 200)))
            (setf data (subseq data 0
                               (read-buf-nonblock data
                                                  (socket-make-stream s))))
            (princ data)
            (> (length data) 0)))
      (network-unreachable-error () 'network-unreachable))
  t)


#+internet-available
(deftest sockopt-receive-buffer
    ;; on Linux x86, the receive buffer size appears to be doubled in the
    ;; kernel: we set a size of x and then getsockopt() returns 2x.
    ;; This is why we compare with >= instead of =
    (handler-case
        (let ((s (http-stream "ww.telent.net" 80 "HEAD /")))
          (setf (sockopt-receive-buffer s) 1975)
          (let ((data (make-string 200)))
            (setf data (subseq data 0
                               (read-buf-nonblock data
                                                  (socket-make-stream s))))
            (and (> (length data) 0)
                 (>= (sockopt-receive-buffer s) 1975))))
      (network-unreachable-error () 'network-unreachable))
  t)

#+(and ipv4-support sb-thread)
(deftest sockopt-close-wait-listen-eof
    (let ((listen-sock (make-instance 'inet-socket :type :stream :protocol :tcp))
          (client-sock (make-instance 'inet-socket :type :stream :protocol :tcp))
          server-sock
          port)
      (unwind-protect
           (progn
             (socket-bind listen-sock #(127 0 0 1) 0)
             (setf port (nth-value 1 (socket-name listen-sock)))
             (socket-listen listen-sock 1)

             (let ((client-connect-thread
                    (sb-thread:make-thread
                     (lambda ()
                       (socket-connect client-sock #(127 0 0 1) port)
                       (socket-close client-sock)))))
               (sb-thread:join-thread client-connect-thread :timeout 20)
               (setf server-sock (socket-accept listen-sock)))

             ;; Wait for input. This should return when we get EOF
             ;; from the client. It should /not/ hang.
             (sb-sys:wait-until-fd-usable (socket-file-descriptor server-sock) :input)
             (listen (socket-make-stream server-sock :input t)))
        (socket-close listen-sock)
        (socket-close client-sock)
        (and server-sock (socket-close server-sock))))
  nil)

#+ipv4-support
(deftest socket-open-p-true.1
    (socket-open-p (make-instance 'inet-socket :type :stream :protocol :tcp))
  t)
#+(and ipv4-support internet-available)
(deftest socket-open-p-true.2
    (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (socket-connect s #(127 0 0 1) 7)
             (socket-open-p s))
        (socket-close s)))
  t)
#+ipv4-support
(deftest socket-open-p-false
    (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
      (socket-close s)
      (socket-open-p s))
  nil)

;;; we don't have an automatic test for some of this yet.  There's no
;;; simple way to run servers and have something automatically connect
;;; to them as client, unless we spawn external programs.  Then we
;;; have to start telling people what external programs they should
;;; have installed.  Which, eventually, we will, but not just yet


;;; to check with this: can display packets from multiple peers
;;; peer address is shown correctly for each packet
;;; packet length is correct
;;; long (>500 byte) packets have the full length shown (doesn't work)

(defun udp-server (port)
  (let ((s (make-instance 'inet-socket :type :datagram :protocol :udp)))
    (socket-bind s #(0 0 0 0) port)
    (loop
     (multiple-value-bind (buf len address port) (socket-receive s nil 500)
       (format t "Received ~A bytes from ~A:~A - ~A ~%"
               len address port (subseq buf 0 (min 10 len)))))))

#+(and ipv4-support sb-thread)
(deftest interrupt-io
    (let (result
          (sem (sb-thread:make-semaphore)))
      (labels
          ((client (port)
             (setf result
                   (let ((s (make-instance 'inet-socket
                                           :type :stream
                                           :protocol :tcp)))
                     (socket-connect s #(127 0 0 1) port)
                     (let ((stream (socket-make-stream s
                                                       :input t
                                                       :output t
                                                       :buffering :none)))
                       (handler-case
                           (prog1
                               (catch 'stop
                                 (sb-thread:signal-semaphore sem)
                                 (read-char stream))
                             (close stream))
                         (error (c)
                           c))))))
           (server ()
             (let ((s (make-instance 'inet-socket
                                     :type :stream
                                     :protocol :tcp)))
               (setf (sockopt-reuse-address s) t)
               (socket-bind s (make-inet-address "127.0.0.1") 0)
               (socket-listen s 5)
               (multiple-value-bind (* port)
                   (socket-name s)
                 (let* ((client (sb-thread:make-thread
                                 (lambda () (client port))))
                        (r (socket-accept s))
                        (stream (socket-make-stream r
                                                    :input t
                                                    :output t
                                                    :buffering :none)))
                   (socket-close s)
                   (sb-thread:wait-on-semaphore sem)
                   (sleep 0.1)
                   (sb-thread:interrupt-thread client
                                               (lambda () (throw 'stop :ok)))
                   (unless (sb-ext:wait-for (null (sb-thread::thread-interruptions client)) :timeout 5)
                     (setf result :timeout))
                   (write-char #\x stream)
                   (close stream)
                   (socket-close r)
                   (sb-thread:join-thread client :timeout 5))))))
        (server))
      result)
  :ok)

(defmacro with-client-and-server (((socket-class &rest common-initargs)
                                   (listen-socket-var &rest listen-address)
                                   (client-socket-var &rest client-address)
                                   server-socket-var)
                                  &body body)
  `(let ((,listen-socket-var (make-instance ',socket-class ,@common-initargs))
         (,client-socket-var (make-instance ',socket-class ,@common-initargs))
         (,server-socket-var))
     (unwind-protect
          (progn
            (setf (sockopt-reuse-address ,listen-socket-var) t)
            (socket-bind ,listen-socket-var ,@listen-address)
            (socket-listen ,listen-socket-var 5)
            (socket-connect ,client-socket-var ,@client-address)
            (setf ,server-socket-var (socket-accept ,listen-socket-var))
            ,@body)
       (socket-close ,client-socket-var)
       (socket-close ,listen-socket-var)
       (when ,server-socket-var
         (socket-close ,server-socket-var)))))

;; For stream sockets, make sure a shutdown of the output direction
;; translates into an END-OF-FILE on the other end, no matter which
;; end performs the shutdown and independent of the element-type of
;; the stream.
#+ipv4-support
(macrolet
    ((define-shutdown-test (name who-shuts-down who-reads element-type direction)
       `(deftest ,name
          (let ((address (make-inet-address "127.0.0.1")))
            (with-client-and-server
                ((inet-socket :protocol :tcp :type :stream)
                 (listener address 0)
                 (client   address (nth-value 1 (socket-name listener)))
                 server)
              (socket-shutdown ,who-shuts-down :direction ,direction)
              (handler-case
                  (sb-ext:with-timeout 2
                    (,(if (eql element-type 'character)
                          'read-char 'read-byte)
                      (socket-make-stream
                       ,who-reads :input t :output t
                       :element-type ',element-type)))
                (end-of-file ()
                  :ok)
                (sb-ext:timeout () :timeout))))
          :ok))
     (define-shutdown-tests (direction)
       (flet ((make-name (name)
                (intern (concatenate
                         'string (string name) "." (string direction)))))
         `(progn
            (define-shutdown-test ,(make-name 'shutdown.server.character)
              server client character ,direction)
            (define-shutdown-test ,(make-name 'shutdown.server.ub8)
              server client (unsigned-byte 8) ,direction)
            (define-shutdown-test ,(make-name 'shutdown.client.character)
              client server character ,direction)
            (define-shutdown-test ,(make-name 'shutdown.client.ub8)
              client server (unsigned-byte 8) ,direction)))))

  (define-shutdown-tests :output)
  (define-shutdown-tests :io))

(defun poor-persons-random-address ()
  (let ((base (expt 36 8)))
    (format nil "~36R" (+ base (random base (make-random-state t))))))

#+linux
(deftest abstract.smoke
    (let* ((address (poor-persons-random-address))
           (message "message")
           (buffer (make-string (length message))))
      (with-client-and-server ((local-abstract-socket :type :stream)
                               (listener address)
                               (client address)
                               server)
        (socket-send client message nil)
        (string= (socket-receive server buffer nil) message)))
  t)

#+linux
(deftest abstract.socket-peername
    (let ((address (poor-persons-random-address)))
      (with-client-and-server ((local-abstract-socket :type :stream)
                               (listener address)
                               (client address)
                               server)
        (string= (sb-ext:octets-to-string (socket-peername client)) address)))
  t)
