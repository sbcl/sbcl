<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN"

--

This is a stylesheet for converting DocBook to HTML, implemented as a
customization layer over Norman Walsh's modular DocBook stylesheets.
It's possible that it could be useful for other documents, or even
that it could be an example of decent DSSSL style, but if so, that's
basically an accident, since it was written based on a superficial
reading of chapter 4 of _DocBook: The Definitive Guide_, by Norman
Walsh and Leonard Muellner, and has only been tested on the SBCL
manual.

This software is part of the SBCL system. See the README file for more
information.

The SBCL system is derived from the CMU CL system, which was written
at Carnegie Mellon University and released into the public domain. The
software is in the public domain and is provided with absolutely no
warranty. See the COPYING and CREDITS files for more information.

KLUDGE: The ENTITY docbook.dsl command is hardwired to the appropriate
location for my OpenBSD 2.9 system. There's got to be a more flexible
way to do it, but I'm not enough of an SGML guru to begin to guess
what it would be. (WHN 2001-10-15)

--

 [<!ENTITY docbook.dsl
           SYSTEM
	   "/usr/local/share/sgml/docbook/dsssl/modular/html/docbook.dsl"
	   CDATA
	   dsssl>]>

<style-sheet>
<style-specification id="html" use="docbook">
<style-specification-body>

;;; FIXME: It would be nice to have output files have ".html" extensions
;;; instead of ".htm" extensions.

;;; Essentially all the stuff in the "Programming languages and
;;; constructs" section (pp. 40-41 of _DocBook: The Definitive Guide_)
;;; is to be monospaced. The one exception is "replaceable", which
;;; needs to be distinguishable from the others.
;;;
;;; (In the modular stylesheets as of 1.54, some elements like "type"
;;; were typeset in the same font as running text, which led to
;;; horrible confusion in the SBCL manual.)
(element action ($mono-seq$))
(element classname ($mono-seq$))
(element constant ($mono-seq$))
(element errorcode ($mono-seq$))
(element errorname ($mono-seq$))
(element errortype ($mono-seq$))
(element function ($mono-seq$))
(element interface ($mono-seq$))
(element interfacedefinition ($mono-seq$))
(element literal ($mono-seq$))
(element msgtext ($mono-seq$))
(element parameter ($mono-seq$))
(element property ($mono-seq$))
(element replaceable ($italic-seq$))
(element returnvalue ($mono-seq$))
(element structfield ($mono-seq$))
(element structname ($mono-seq$))
(element symbol ($mono-seq$))
(element token ($mono-seq$))
(element type ($mono-seq$))
(element varname ($mono-seq$))

;;; Things in the "Operating systems" and "General purpose"
;;; sections (pp. 41-42 and pp. 42-43
;;; of _DocBook: The Definitive Guide_) are handled on a case
;;; by case basis.
;;;
;;; "Operating systems" section
(element application ($charseq$))
(element command ($mono-seq$))
(element envar ($mono-seq$))
(element filename ($mono-seq$))
(element medialabel ($mono-seq$))
;;; (The "msgtext" element is handled in another section.)
(element option ($mono-seq$))
;;; (The "parameter" element is handled in another section.)
(element prompt ($bold-mono-seq$))
(element systemitem ($mono-seq$))
;;;
;;; "General purpose" section
(element database ($charseq$))
(element email ($mono-seq$))
;;; (The "filename" element is handled in another section.)
(element hardware ($mono-seq$))
(element inlinegraphic ($mono-seq$))
;;; (The "literal" element is handled in another section.)
;;; (The "medialabel" element is handled in another section.)
;;; (The "option" element is handled in another section.)
(element optional ($italic-mono-seq$))
;;; (The "replaceable" element is handled in another section.)
;;; (The "symbol" element is handled in another section.)
;;; (The "token" element is handled in another section.)
;;; (The "type" element is handled in another section.)

</style-specification-body>
</style-specification>

<external-specification id="docbook" document="docbook.dsl">

</style-sheet>
