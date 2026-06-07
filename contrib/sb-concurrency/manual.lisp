(in-package :sb-manual)

(defsection @sb-concurrency (:title "sb-concurrency")
  "Additional data structures, synchronization primitives and tools for
  concurrent programming. Similiar to Java's `java.util.concurrent`
  package."
  (@sb-concurrency-queue section)
  (@sb-concurrency-mailbox section)
  (@sb-concurrency-gates section)
  (@sb-concurrency-frlocks section))

(defsection @sb-concurrency-queue (:title "Queue")
  "SB-CONCURRENCY:QUEUE is a lock-free, thread-safe FIFO queue
  datatype.

  The implementation is based on _An Optimistic Approach to Lock-Free
  FIFO Queues_ by Edya Ladan-Mozes and Nir Shavit.

  Before SBCL 1.0.38, this implementation resided in its own contrib
  (see @SB-QUEUE), which is still provided for
  backwards-compatibility, but which has since been deprecated."
  (sb-concurrency:queue structure)
  (sb-concurrency:dequeue function)
  (sb-concurrency:enqueue function)
  (sb-concurrency:list-queue-contents function)
  (sb-concurrency:make-queue function)
  (sb-concurrency:queue-count function)
  (sb-concurrency:queue-empty-p function)
  (sb-concurrency:queue-name function)
  (sb-concurrency:queuep function))

(defsection @sb-concurrency-mailbox (:title "Mailbox (lock-free)")
  "SB-CONCURRENCY:MAILBOX is a lock-free message queue where one or
  multiple ends can send messages to one or multiple receivers. The
  difference to @SB-CONCURRENCY-QUEUE is that the receiving end may
  block until a message arrives.

  Built on top of the @SB-CONCURRENCY-QUEUE implementation."
  (sb-concurrency:mailbox structure)
  (sb-concurrency:list-mailbox-messages function)
  (sb-concurrency:mailbox-count function)
  (sb-concurrency:mailbox-empty-p function)
  (sb-concurrency:mailbox-name function)
  (sb-concurrency:mailboxp function)
  (sb-concurrency:make-mailbox function)
  (sb-concurrency:receive-message function)
  (sb-concurrency:receive-message-no-hang function)
  (sb-concurrency:receive-pending-messages function)
  (sb-concurrency:send-message function))

(defsection @sb-concurrency-gates (:title "Gates")
  "SB-CONCURRENCY:GATE is a synchronization object suitable for when
  multiple threads must wait for a single event before proceeding."
  (sb-concurrency:gate structure)
  (sb-concurrency:close-gate function)
  (sb-concurrency:gate-name function)
  (sb-concurrency:gate-open-p function)
  (sb-concurrency:gatep function)
  (sb-concurrency:make-gate function)
  (sb-concurrency:open-gate function)
  (sb-concurrency:wait-on-gate function))

(defsection @sb-concurrency-frlocks (:title "Frlocks, aka Fast Read Locks")
  (sb-concurrency:frlock structure)
  (sb-concurrency:frlock-read macro)
  (sb-concurrency:frlock-write macro)
  (sb-concurrency:make-frlock function)
  (sb-concurrency:frlock-name function)
  (sb-concurrency:frlock-read-begin function)
  (sb-concurrency:frlock-read-end function)
  (sb-concurrency:grab-frlock-write-lock function)
  (sb-concurrency:release-frlock-write-lock function))
