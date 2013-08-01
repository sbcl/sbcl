(defpackage "SB-BSD-SOCKETS-TEST"
  (:use "CL" "SB-BSD-SOCKETS" "SB-RT"))

(in-package :sb-bsd-sockets-test)

(defmacro deftest* ((name &key fails-on) form &rest results)
  `(progn
     (when (sb-impl::featurep ',fails-on)
       (pushnew ',name sb-rt::*expected-failures*))
     (deftest ,name ,form ,@results)))

;;; a real address
(deftest make-inet-address
  (equalp (make-inet-address "127.0.0.1")  #(127 0 0 1))
  t)
;;; and an address with bit 8 set on some octets
(deftest make-inet-address2
  (equalp (make-inet-address "242.1.211.3")  #(242 1 211 3))
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
(deftest get-protocol-by-name/error
  (handler-case (get-protocol-by-name "nonexistent-protocol")
    (unknown-protocol ()
      t)
    (:no-error ()
      nil))
  t)

(deftest make-inet-socket
  ;; make a socket
  (let ((s (make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "tcp"))))
    (and (> (socket-file-descriptor s) 1) t))
  t)

(deftest make-inet-socket-keyword
    ;; make a socket
    (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
      (and (> (socket-file-descriptor s) 1) t))
  t)

(deftest* (make-inet-socket-wrong)
    ;; fail to make a socket: check correct error return.  There's no nice
    ;; way to check the condition stuff on its own, which is a shame
    (handler-case
        (make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "udp"))
      ;; CLH FIXME! some versions of darwin just return a socket error
      ;; here, not socket-type-not-supported-error or
      ;; protocol-not-supported-error.
      ((or #+darwin socket-error
        socket-type-not-supported-error
        protocol-not-supported-error)
          (c)
        (declare (ignorable c)) t)
      (:no-error nil))
  t)

(deftest* (make-inet-socket-keyword-wrong)
    ;; same again with keywords
    (handler-case
        (make-instance 'inet-socket :type :stream :protocol :udp)
      ;; CLH FIXME! some versions of darwin just return a socket error
      ;; here, not socket-type-not-supported-error or
      ;; protocol-not-supported-error.
      ((or
        #+darwin socket-error
        protocol-not-supported-error
        socket-type-not-supported-error)
          (c)
        (declare (ignorable c)) t)
      (:no-error nil))
  t)


(deftest* (non-block-socket)
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (non-blocking-mode s) t)
    (non-blocking-mode s))
  t)

(deftest inet-socket-bind
  (let* ((tcp (get-protocol-by-name "tcp"))
         (address (make-inet-address "127.0.0.1"))
         (s1 (make-instance 'inet-socket :type :stream :protocol tcp))
         (s2 (make-instance 'inet-socket :type :stream :protocol tcp)))
    (unwind-protect
         ;; Given the functions we've got so far, if you can think of a
         ;; better way to make sure the bind succeeded than trying it
         ;; twice, let me know
         (progn
           (socket-bind s1 address 0)
           (handler-case
               (let ((port (nth-value 1 (socket-name s1))))
                 (socket-bind s2 address port)
                 nil)
             (address-in-use-error () t)))
      (socket-close s1)
      (socket-close s2)))
  t)

(deftest* (simple-sockopt-test)
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
(deftest get-host-by-name
  (equalp (car (host-ent-addresses (get-host-by-name "a.root-servers.net")))
          #(198 41 0 4))
  t)

#+internet-available
(deftest get-host-by-address
    (host-ent-name (get-host-by-address #(198 41 0 4)))
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

(deftest socket-open-p-true.1
    (socket-open-p (make-instance 'inet-socket :type :stream :protocol :tcp))
  t)
#+internet-available
(deftest socket-open-p-true.2
    (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (socket-connect s #(127 0 0 1) 7)
             (socket-open-p s))
        (socket-close s)))
  t)
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

#+sb-thread
(deftest interrupt-io
    (let (result)
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
                                 (progn
                                   (read-char stream)
                                   (sleep 0.1)
                                   (sleep 0.1)
                                   (sleep 0.1)))
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
                                                    :buffering :none))
                        (ok :ok))
                   (socket-close s)
                   (sleep 5)
                   (sb-thread:interrupt-thread client
                                               (lambda () (throw 'stop ok)))
                   (sleep 5)
                   (setf ok :not-ok)
                   (write-char #\x stream)
                   (close stream)
                   (socket-close r))))))
        (server))
      result)
  :ok)

(defmacro with-client-and-server ((server-socket-var client-socket-var) &body body)
  (let ((listen-socket (gensym "LISTEN-SOCKET")))
    `(let ((,listen-socket (make-instance 'inet-socket
                                          :type :stream
                                          :protocol :tcp))
           (,client-socket-var (make-instance 'inet-socket
                                              :type :stream
                                              :protocol :tcp))
           (,server-socket-var))
      (unwind-protect
           (progn
             (setf (sockopt-reuse-address ,listen-socket) t)
             (socket-bind ,listen-socket (make-inet-address "127.0.0.1") 0)
             (socket-listen ,listen-socket 5)
             (socket-connect ,client-socket-var (make-inet-address "127.0.0.1")
                             (nth-value 1 (socket-name ,listen-socket)))
             (setf ,server-socket-var (socket-accept ,listen-socket))
             ,@body)
        (socket-close ,client-socket-var)
        (socket-close ,listen-socket)
        (when ,server-socket-var
          (socket-close ,server-socket-var))))))

;; For stream sockets, make sure a shutdown of the output direction
;; translates into an END-OF-FILE on the other end, no matter which
;; end performs the shutdown and independent of the element-type of
;; the stream.
(macrolet
    ((define-shutdown-test (name who-shuts-down who-reads element-type direction)
       `(deftest ,name
          (with-client-and-server (client server)
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
              (sb-ext:timeout () :timeout)))
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
