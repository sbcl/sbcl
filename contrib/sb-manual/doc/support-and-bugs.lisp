(in-package :sb-manual)

(defsection @support-and-bugs (:title "Getting Support and Reporting Bugs")
  (@volunteer-support section)
  (@commercial-support section)
  (@reporting-bugs section))

(defsection @volunteer-support (:title "Volunteer Support")
  "Your primary source of SBCL support should probably be the mailing
  list `sbcl-help`: in addition to other users SBCL developers monitor
  this list and are available for advice. As an anti-spam measure
  subscription is required for posting:

  <https://lists.sourceforge.net/lists/listinfo/sbcl-help>

  Remember that the people answering your question are volunteers, so
  you stand a much better chance of getting a good answer if you ask a
  good question.

  Before sending mail, check the list archives at either

  <http://sourceforge.net/mailarchive/forum.php?forum_name=sbcl-help>

  or

  <http://news.gmane.org/gmane.lisp.steel-bank.general>

  to see if your question has been answered already. Checking the bug
  database is also worth it (see @REPORTING-BUGS), to see if the issue
  is already known.

  For general advice on asking good questions, see

  <http://www.catb.org/~esr/faqs/smart-questions.html>.")

(defsection @commercial-support (:title "Commercial Support")
  "There is no formal organization developing SBCL, but if you need a
  paid support arrangement or custom SBCL development, we maintain the
  list of companies and consultants below. Use it to identify service
  providers with appropriate skills and interests, and contact them
  directly.

  The SBCL project cannot verify the accuracy of the information or
  the competence of the people listed, and they have provided their
  own blurbs below: you must make your own judgement of suitability
  from the available information - refer to the links they provide,
  the CREDITS file, mailing list archives, CVS commit messages, and so
  on. Please feel free to ask for advice on the sbcl-help list.

  (At present, no companies or consultants wish to advertise paid
  support or custom SBCL development in this manual).")

(defsection @reporting-bugs (:title "Reporting Bugs")
  "SBCL uses Launchpad to track bugs. The bug database is available at

  <https://bugs.launchpad.net/sbcl>

  Reporting bugs there requires registering at Launchpad. However,
  bugs can also be reported on the mailing list `sbcl-bugs`,
  which is moderated but does _not_ require subscribing.

  Simply send email to `sbcl-bugs@lists.sourceforge.net` and the bug
  will be checked and added to Launchpad by SBCL maintainers."
  (@how-to-report-bugs-effectively section)
  (@how-to-report-signal-related-bugs section))

(defsection @how-to-report-bugs-effectively
    (:title "How to Report Bugs Effectively")
  "Please include enough information in a bug report that someone reading
  it can reproduce the problem, i.e. don't write

      Subject: apparent bug in PRINT-OBJECT (or *PRINT-LENGTH*?)
      PRINT-OBJECT doesn't seem to work with *PRINT-LENGTH*. Is this a bug?

  but instead

      Subject: apparent bug in PRINT-OBJECT (or *PRINT-LENGTH*?)
      In sbcl-1.2.3 running under OpenBSD 4.5 on my Alpha box, when
      I compile and load the file
         (DEFSTRUCT (FOO (:PRINT-OBJECT (LAMBDA (X Y)
                                          (LET ((*PRINT-LENGTH* 4))
                                            (PRINT X Y)))))
           X Y)
      then at the command line type
         (MAKE-FOO)
      the program loops endlessly instead of printing the object.

  A more in-depth discussion on reporting bugs effectively can be
  found at

  <http://www.chiark.greenend.org.uk/~sgtatham/bugs.html>.")

(defsection @how-to-report-signal-related-bugs
    (:title "How to Report Signal-related Bugs")
  "If you run into a signal related bug, you are getting fatal errors
  such as `signal N is [un]blocked` or just hangs, and you want to
  send a useful bug report then:

  - Compile SBCL with @LDB enabled (feature `:SB-LDB`, see
    `base-target-features.lisp-expr`).

  - Isolate a smallish test case, run it.

  - If it just hangs kill it with `SIGABRT`: `kill -ABRT <pidof sbcl>`.

  - Print the backtrace from ldb by typing `ba`.

  - Attach gdb: `gdb -p <pidof sbcl>` and get backtraces for all
    threads: `thread apply all ba`.

  - If multiple threads are in play then still in gdb, try to get Lisp
    backtrace for all threads: `thread apply all call
    backtrace_from_fp($ebp, 100, 0)`. Substitute `$ebp` with `$rbp` on
    x86-64. The backtraces will appear in the stdout of the SBCL
    process.

  - Send a report with the backtraces and the output (both stdout and
    stderr) produced by SBCL.

  - Don't forget to include OS and SBCL version.

  - If available, include information on outcome of the same test with
    other versions of SBCL, OS, ...")
