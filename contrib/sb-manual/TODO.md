# How/when to load/include docs of contribs?

Currently, `SB-MANUAL` loads *all* contribs to be able to query the
definition docstrings. Each contrib directory has a `manual.lisp`
file, which is part of the `SB-MANUAL` contrib (the files are
symlinked).

On the positive side, this does not load extra stuff until the user
`REQUIRE`s `SB-MANUAL`. However, then it loads all contribs.

A finer grained approach may be preferable. For example, we could make
the `manual.lisp` file part of the contrib itself. Then people might
complain about the overhead of loading/having the docstrings in the
image.

Alternatively, we could have `sb-bsd-sockets/manual.lisp` as a new
`SB-BSD-SOCKETS-MANUAL` module. Eh.

# SWITCH-TO-PAX automatically?

# How to deal with repetitive package names?

For example, `SB-ALIEN` is `:USE`d by `SB-MANUAL` so that the section
docstrings need not fully qualify with `SB-ALIEN:` a thousand times.
In the generated Texinfo, this can be a tad confusing. In output
formats with links (e.g. HTML from PAX), this is clearly preferable.

Nicknames, maybe?

# Implement PAX reflinks, e.g. `[function][type]`

See `PAX::@REFLINKS`.
