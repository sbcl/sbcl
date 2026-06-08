Docstring Style Guide
---------------------

The Markdown-to-Texinfo converter is documented in `markdown.lisp`.
Here, we provide a quick howto and style guide. If something is
unclear (there should be lots), then you can test things with e.g.

    (sb-manual::markdown-to-texinfo "PRINT")

## Writing Names Inline

- Upcase symbols naming definitions (or arguments of the function
  being documented):

        SB-EXT:EXIT
        PRINT
        X

    Unqualified symbols must be accessible in the package where the
    docstring is parsed. A good practice is to rely on Slime's TAB
    completion on the name; then it's clear that the symbol exists.

- Upcase and mark strings naming definitions as code:

        `SB-EXT`

    This is necessary because we don't want the creation of package
    `ANSI` to automatically codify all occurrences of `ANSI` in the
    documentation.

- Upcase and mark non-existent symbols as code:

        `WIN64::WHATEVER`

    For when the code that defines the package or the symbol cannot be
    loaded.

- Mark C symbols as code:

        `send`
        `send(2)`

    The latter is preferable to `send`(2), as it can be parsed by
    `manual-entry` in Emacs.

    If there is a Lisp symbol `SEND` accessible in the current package
    and it names a definition, then you need to prevent it from being
    autolinked (only with PAX, currently):

        `\send(2)`

    > _Note_: Here and in general, the backslashes need to be doubled
    > when writing docstrings.

- Mark C constants as code and escape downcasing:

       `\\AF_LOCAL`

    All capital code is downcased, which is great in the common case
    but not here.

## Writing Sexps Inline

- One option is to mark the whole expression as code:

        `(PRINT 'HELLO)`

    Here, everything is rendered in monospace, downcased, no autolinks.

- Alternatively, one could simply write

        (PRINT '`HELLO`)

    and rely on automatic codification for `PRINT`, codify `HELLO`
    manually, and accept the fact that the parens will be in the
    proportional font. In return, `PRINT` is autolinked.

- Even more alternatively:

        `(`PRINT` 'HELLO)`

    Here, everything is monospace (except maybe the space), and
    `PRINT` is autolinked.

## Writing Signatures Inline

Follow the somewhat usual `<TERMINAL-NAME>` convention:

    One option is `(:USE <PACKAGE-NAME>*)`, where `<PACKAGE-NAME>`
    is a package designator.

If you wrote the mixed-case `(:USE <package-name>*)`, then the
expression would be downcased, which is bad here.

## Code Blocks

In code blocks, always write as you would write in a source file,
which means downcase the symbols. Prefer indented code blocks

```
    this is indented
```

to fenced code blocks:

    ```
    this is fenced
    ```

Use fenced code blocks only if there multiple successive code blocks
that you definitely want to render as distinct "boxes".

One gotcha to look out for is indenting code blocks is in list items:

```
- this is a list

    A separate child paragraph indented to be "within" the list item.

        (some code)
```

Note how the code block is indented 8 spaces from the `-` character.
The required indentation remains the same if the child paragraph above
is not present.

## Links

### Plain Links

For short links:

    <http://x.y>

### Explicit Links

For overly long links:

    [label](http://x.y/a?b=c)

When generating Texinfo, this is translated to `@url`, which is
rendered like a HTML link in HTML and PDF, but with `label (see
<uri>)` in Info.

PAX will generate a normal link except in plain text, where it simply
drops the URI.

### Reflinks

In PAX-generated output only, `FUNCTION` is autolinked to its
definitions. If there is more than one, you can dismambiguate:

    [FUNCTION][type]

PAX will produce a single link in this case. If `(FUNCTION CLASS)` is
documented in some section, then the link will point to that
documentation. Else, the link will go to the CLHS. The latter can be
forced:

    [FUNCTION][(clhs type)]

Since we don't link to definitions in Texinfo (except to sections),
the above examples are translated to `FUNCTION` there.

## Notes

- In many cases, just write a sentence:

        Note that this may not be always so.

- To add a note, use blockquotes:

        > _Note_: This is not terribly important,
        > but it can span multiple lines.

    Blockquotes render indented in most formats, sometimes with a
    vertical line to their left (e.g. Markdown on GitHub).

- To add a warning, use blockquotes:

        > __Warning__: Beware of the dog!

Blockquotes without `_Note_` and `__Warning__` are fine for their
other purposes: citing stuff, add copyright notices, etc. Markdown is
not semantic. Use it freely and visually.

Footnotes are not supported.

## Typographic Miscellanea

The Markdown-to-Texinfo converter does not convert e.g. `--` to
en-dash. This is to prevent messing up the output when something like
`git <option>* -- <path>` is written without proper markup. The issue
is similar but maybe more pronounced with literal strings and curly
quotes: you don't want to copy the rendered version of

    (print "Hello, world")

to the REPL, only to find the double quotes have been replaced by some
fancy characters.

## Inline Quotes and Emphasis

Use strong emphasis (e.g. `__not__`) very sparingly. It usually gets
rendered bold, which draws too much attention. However, emphasis
(usually italic) is fine.

You may use emphasis instead of single or double quotes:

          CLHS `14.1` says that _depending on context, a group of
          connected conses can be viewed in a variety of different
          ways_, but that's trivial.

You may also use double quotes, but the straight double quotes are
kind of ugly in proportional fonts.

## Sections

### Titles

Use title case:

    Big and Bigger Words

### Names

Name the section `@BIG-AND-BIGGER-WORDS` or something shorter like
`@BIG-WORDS`. In a docstring, you may read `"@BIG-WORDS are
necessary"`, and `"Big and Bigger Words are necessary"` when it's
rendered, so make sure that they are similar enough.

Note that section names live in a flat namespace: they are all
exported from SB-MANUAL, and they also show up the Texinf indices
without the context of their parents. So, their names should be
globally meaningful. Thus, it is better to name the section describing
`SB-ACLREPL`'s usage `@SB-ACLREPL-USAGE` than `@USAGE`.

## Docstring Formatting

The Emacs style:

    (defun foo (x)
      "Return X.
    It signals no errors."
      x)

You may also indent all but the first line as long as you do it
consistently within the docstring. Maybe one day we could even agree
upon a universally accepted style ... just joking.

## Locale Stuff

Currently the manual is a mix of American and British English.

In the vast majority of the cases, the existing docstrings use `e.g.
X` and `i.e. X` (the British version). For consistency, do not use
other forms such as `eg. X` or `i.e., X`.

## Paragraph Formatting

Note that docstrings are also inconsistent about whether one or two
spaces follow a full stop (controlled by `sentence-end-double-space`
in Emacs) and their `fill-column`. It would be nice to have them all
`fill-paragraph`ed with the same settings ...
