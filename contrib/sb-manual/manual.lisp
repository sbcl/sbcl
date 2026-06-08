(in-package :sb-manual)

(defsection @sb-manual (:title "sb-manual")
  "The `SB-MANUAL` module has the SBCL user manual in forms
  mimicking `PAX:DEFSECTION`:

      (defsection @example (:title \"Example\")
        \"This is an example, but see the real @SB-MANUAL.\"
        (print function)
        (@subexample section))

  The names of the variables holding the documentation are exported
  from the `SB-MANUAL` package. Since sections are basically
  variables, in Slime, `\\\\M-.` on `\"@SB-MANUAL\"`, `\"print\"`, or
  on `\"@subexample\"` will take you to the respective definition.
  This makes it easy to navigate the documentation. Normal Lisp
  definition docstrings and section docstrings reference sections
  following the usual convention of uppercasing the name. Docstrings
  are in a subset of Markdown and use very little markup in general,
  so they are easy to read directly in the source.

  The official manual in Info, HTML and PDF formats is generated via
  Texinfo generated from these definitions."
  (@using-pax section)
  (@browsing-live-with-pax section)
  (@fancy-documentation-with-pax section))

(defsection @using-pax (:title "Using \\PAX")
  "However, `SB-MANUAL::DEFSECTION` is but a dummy implementation of
  `PAX:DEFSECTION` to avoid a hard dependency on \\PAX.

  See the `MGL-PAX` ASDF:SYSTEM or <https://github.com/melisgl/mgl-pax/>.

  When \\PAX is loaded, the dummy `DEFSECTION` definitions are made
  real, so that \\PAX can work with them."
  (use-pax function))

(defsection @browsing-live-with-pax (:title "Browsing Live with \\PAX")
  "With \\PAX, you can browse the manual live. The documentation of this
  feature is available at
  <https://melisgl.github.io/mgl-pax-world/pax-manual.html#MGL-PAX:@BROWSING-LIVE-DOCUMENTATION%20MGL-PAX:SECTION>.

  If you are browsing this manual live right now, here is the
  equivalent live link: `PAX::@BROWSING-LIVE-DOCUMENTATION`.

  Notable features:

  - Autolinks within the manual: if SB-EXT:EXIT is mentioned, then
    it's linked to its documentation. You basically get links to where
    `\\\\M-.` would go in the sources.

  - Autolinks to the \\CLHS.

  - View the documentation of any Lisp definition or section without
    generating the entire manual.

  - Locatives (e.g. the `\"[function]\"` in `\"- [function]
    SB-EXT:EXIT\"`) are also links in live browsing: they tell Slime
    to visit the definition.

      For this to work, you need to allow Slime to evaluate Elisp sent
      from SBCL:

          (setq slime-enable-evaluate-in-emacs t)

      and maybe your window manager focus stealing configuration needs
      tweaking as well.

  Live browsing can greatly reduce the latency of Edit-Compile-View
  Loop, when working on documentation.")

(defsection @fancy-documentation-with-pax
    (:title "Fancy Documentation with \\PAX")
  "\\PAX can generate dead documentation, too. In the SBCL sources,
  `contrib/sb-manual/make-pax-docs.sh` generates the manual in plain
  text, Markdown, PDF, and HTML formats. These differ from those
  generated via Texinfo in that they are autolinked (like when
  @BROWSING-LIVE-WITH-PAX).

  Also, you can generate documentation yourself with e.g.

      (pax:document sb-manual:@sbcl-manual :format :markdown)")
