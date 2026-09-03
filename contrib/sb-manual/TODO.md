- How/when to load/include docs of contribs?

    Currently, `sb-manual` loads *all* contribs to be able to query
    the definition docstrings. Each contrib directory has a
    `manual.lisp` file, which is part of the `sb-manual` contrib.

    On the positive side, this does not load extra stuff until the
    user `require`s `sb-manual`. However, then it loads all contribs.

    It shouldn't be difficult to show documtation only for actually
    loaded contribs.

- How to deal with repetitive package names?

    For example, `sb-alien` is `:use`d by `sb-manual` so that the
    section docstrings need not fully qualify with `sb-alien:` a
    thousand times. In the generated Texinfo, this can be a tad
    confusing. In output formats with links (e.g. HTML from PAX), this
    is clearly preferable.

    Nicknames, maybe?

- Improve section names

    They are a soft interface: exported from `sb-manual` and visible
    to the user via HTML anchors.
