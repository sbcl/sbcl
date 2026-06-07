(in-package :sb-manual)

(defsection @sb-simple-streams (:title "Simple Streams")
  "Simple streams are an extensible streams protocol that avoids some
  problems with @GRAY-STREAMS.

  Documentation about simple streams is available at:

  <http://www.franz.com/support/documentation/6.2/doc/streams.htm>

  The implementation should be considered Alpha-quality; the basic
  framework is there, but many classes are just stubs at the moment.

  See `\\\\SYS:CONTRIB;SB-SIMPLE-STREAMS;SIMPLE-STREAM-TEST.LISP` for
  things that should work.

  Known differences to the ACL behaviour:

  - `SB-SIMPLE-STREAMS:OPEN` does not return a `SIMPLE-STREAM` by
    default. See its :CLASS argument.

  - `WRITE-VECTOR` is unimplemented.")
