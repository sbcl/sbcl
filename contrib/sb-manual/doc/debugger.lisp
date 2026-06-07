(in-package :sb-manual)

(defsection @debugger (:title "Debugger")
  "This chapter documents the debugging facilities of SBCL, including
  the debugger, single-stepper and TRACE, and the effect of `(OPTIMIZE
  DEBUG)` declarations."
  (@debugger-entry section)
  (@debugger-command-loop section)
  (@stack-frames section)
  (@variable-access section)
  (@source-location-printing section)
  (@debugger-policy-control section)
  (@exiting-commands section)
  (@information-commands section)
  (@breakpoint-commands section)
  (@function-tracing section)
  (@single-stepping section)
  (@enabling-and-disabling-the-debugger section))

(defsection @debugger-entry (:title "Debugger Entry")
  (@debugger-banner section)
  (@debugger-invocation section))

(defsection @debugger-banner (:title "Debugger Banner")
  "When you enter the debugger, it looks something like this:

      debugger invoked on a TYPE-ERROR in thread 11184:
        The value 3 is not of type LIST.

      You can type HELP for debugger help, or (SB-EXT:QUIT) to exit from SBCL.

      restarts (invokable by number or by possibly-abbreviated name):
        0: [ABORT   ] Reduce debugger level (leaving debugger, returning to toplevel).
        1: [TOPLEVEL] Restart at toplevel READ/EVAL/PRINT loop.
      (CAR 1 3)
      0]

  The first group of lines describe what the error was that put us in
  the debugger. In this case CAR was called on `3`, causing a
  TYPE-ERROR.

  This is followed by the \"beginner help line\", which appears only
  if SB-DEBUG:*DEBUG-BEGINNER-HELP-P* is true (default).

  Next comes a listing of the active restart names, along with their
  descriptions -- the ways we can restart execution after this error.
  In this case, both options return to top-level. Restarts can be
  selected by entering the corresponding number or name.

  The current frame appears right underneath the restarts, immediately
  followed by the debugger prompt.")

(defsection @debugger-invocation (:title "Debugger Invocation")
  "The debugger is invoked when:

  - ERROR is called, and the condition it signals is not handled.

  - BREAK is called, or SIGNAL is called with a condition that matches
    the current *BREAK-ON-SIGNALS*.

  - The debugger is explicitly entered with the INVOKE-DEBUGGER
    function.

  When the debugger is invoked by a condition, ANSI mandates that the
  value of *DEBUGGER-HOOK*, if any, be called with two arguments: the
  condition that caused the debugger to be invoked and the previous
  value of *DEBUGGER-HOOK*. When this happens, *DEBUGGER-HOOK* is
  bound to NIL to prevent recursive errors. However, ANSI also
  mandates that *DEBUGGER-HOOK* not be invoked when the debugger is to
  be entered by the BREAK function. For users who wish to provide an
  alternate debugger interface (and thus catch BREAK entries into the
  debugger), SBCL provides SB-EXT:*INVOKE-DEBUGGER-HOOK*, which is
  invoked during any entry into the debugger."
  ;; When Swank is loaded, it sets this variable.
  (sb-ext:*invoke-debugger-hook* (variable nil)))

(defsection @debugger-command-loop (:title "Debugger Command Loop")
  "The debugger is an interactive read-eval-print loop much like the
  normal top level, but some symbols are interpreted as debugger
  commands instead of being evaluated. A debugger command starts with
  the symbol name of the command, possibly followed by some arguments
  on the same line. Some commands prompt for additional input.
  Debugger commands can be abbreviated by any unambiguous prefix:
  `help` can be typed as `h`, `he`, etc.

  The package is not significant in debugger commands; any symbol with
  the name of a debugger command will work. If you want to show the
  value of a variable that happens also to be the name of a debugger
  command you can wrap the variable in a PROGN to hide it from
  the command loop.

  The debugger prompt is `<frame>]`, where `<frame>` is the number of
  the current frame. Frames are numbered starting from zero at the
  top (most recent call), increasing down to the bottom. The current
  frame is the frame that commands refer to.

  It is possible to override the normal printing behaviour in the
  debugger by using the SB-EXT:*DEBUG-PRINT-VARIABLE-ALIST*."
  (sb-ext:*debug-print-variable-alist* variable))

(defsection @stack-frames (:title "Stack Frames")
  "A _stack frame_ is the run-time representation of a call to a
  function; the frame stores the state that a function needs to
  remember what it is doing. Frames have:

  - _Variables_ (see @VARIABLE-ACCESS), which are the values being
    operated on.

  - _Arguments_ to the call (which are really just particularly
    interesting variables).

  - A current source location (@SOURCE-LOCATION-PRINTING), which is
    the place in the program where the function was running when it
    stopped to call another function, or because of an interrupt or
    error."
  (@stack-motion section)
  (@how-arguments-are-printed section)
  (@function-names section)
  (@debug-tail-recursion section)
  (@unknown-locations-and-interrupts section))

(defsection @stack-motion (:title "Stack Motion")
  "These commands move to a new stack frame and print the name of the
  function and the values of its arguments in the style of a Lisp
  function call:

  - `up`: Move up to the next higher frame. More recent function calls
     are considered to be higher on the stack.

  - `down`: Move down to the next lower frame.

  - `top`: Move to the highest frame, that is, the frame where the
    debugger was entered.

  - `bottom`: Move to the lowest frame.

  - `frame [<n>]`: Move to the frame with the specified number.
    Prompts for the number if not supplied. The frame with number 0 is
    the frame where the debugger was entered.")

(defsection @how-arguments-are-printed (:title "How Arguments are Printed")
  "A frame is printed to look like a function call, but with the actual
  argument values in the argument positions.  So the frame for this call
  in the source:

      (myfun (+ 3 4) 'a)

  would look like this:

      (MYFUN 7 A)

  All keyword and optional arguments are displayed with their actual
  values; if the corresponding argument was not supplied, the value will
  be the default.  So this call:

      (subseq \"foo\" 1)

  would look like this:

      (SUBSEQ \"foo\" 1 3)

  And this call:

      (string-upcase \"test case\")

  would look like this:

      (STRING-UPCASE \"test case\" :START 0 :END NIL)

  The arguments to a function call are displayed by accessing the
  argument variables. Although those variables are initialized to the
  actual argument values, they can be set inside the function; in this
  case the new value will be displayed.

  &REST arguments are handled somewhat differently. The value of the
  rest argument variable is displayed as the spread-out arguments to
  the call, so:

      (format t \"~A is a ~A.\" \"This\" 'test)

  would look like this:

      (FORMAT T \"~A is a ~A.\" \"This\" 'TEST)

  Rest arguments cause an exception to the normal display of keyword
  arguments in functions that have both &REST and &KEY arguments. In
  this case, the keyword argument variables are not displayed at all;
  the rest arg is displayed instead. So for these functions, only the
  keywords actually supplied will be shown, and the values displayed
  will be the argument values, not values of the
  (possibly modified) variables.

  If the variable for an argument is never referenced by the function,
  it will be deleted. The variable value is then unavailable, so the
  debugger prints `#<unused-arg>` instead of the value. Similarly, if
  for any of a number of reasons the value of the variable is
  unavailable or not known to be available (@VARIABLE-ACCESS), then
  `#<unavailable-arg>` will be printed instead of the argument value.

  Note that inline expansion and open-coding affect what frames are
  present in the debugger, see @DEBUGGER-POLICY-CONTROL."
  ;; FIXME: Link here to section about open coding once it exists.
  )

(defsection @function-names (:title "Function Names")
  "If a function is defined by DEFUN it will appear in backtrace
  by that name. Functions defined by LABELS and FLET will appear as
  `(FLET <NAME>)` and `(LABELS <NAME>)` respectively. Anonymous
  lambdas will appear as `(LAMBDA <LAMBDA-LIST>)`."
  (@entry-point-details section))

(defsection @entry-point-details (:title "Entry Point Details")
  "Sometimes the compiler introduces new functions that are used to
  implement a user function, but are not directly specified in the
  source. This is mostly done for argument type and count checking.

  With recursive or block compiled functions, an additional `external`
  frame may appear before the frame representing the first call to the
  recursive function or entry to the compiled block. This is a
  consequence of the way the compiler works: there is nothing odd with
  your program. You may also see `cleanup` frames during the execution
  of UNWIND-PROTECT cleanup code, and `optional` for variable argument
  entry points.")

(defsection @debug-tail-recursion (:title "Debug Tail Recursion")
  "The compiler is _properly tail recursive_. If a function call is
  in a tail-recursive position, the stack frame will be deallocated
  _at the time of the call_, rather than after the call returns.
  Consider this backtrace:

      (BAR ...)
      (FOO ...)

  Because of tail recursion, it is not necessarily the case that `FOO`
  directly called `BAR`. It may be that `FOO` called some other
  function `FOO2`, which then called `BAR` tail-recursively, as in
  this example:

      (defun foo ()
        ...
        (foo2 ...)
        ...)

      (defun foo2 (...)
        ...
        (bar ...))

      (defun bar (...)
        ...)

  Usually the elimination of tail-recursive frames makes debugging
  more pleasant, since these frames are mostly uninformative. If there
  is any doubt about how one function called another, it can usually
  be eliminated by finding the source location in the calling frame.
  See @SOURCE-LOCATION-PRINTING.

  The elimination of tail-recursive frames can be prevented by
  disabling tail-recursion optimization, which happens when the DEBUG
  optimization quality is greater than 2. See
  @DEBUGGER-POLICY-CONTROL."
  ;; FIXME: reinstate this link once the chapter is in the manual. For
  ;; a more thorough discussion of tail recursion, see @TAIL-RECURSION.
  )

(defsection @unknown-locations-and-interrupts
    (:title "Unknown Locations and Interrupts")
  "The debugger operates using special debugging information attached to
  the compiled code. This debug information tells the debugger what it
  needs to know about the locations in the code where the debugger can
  be invoked. If the debugger somehow encounters a location not
  described in the debug information, then it is said to be _unknown_.
  If the code location for a frame is unknown, then some variables may
  be inaccessible, and the source location cannot be precisely
  displayed.

  There are three reasons why a code location could be unknown:

  - There is inadequate debug information due to the value of the
    DEBUG optimization quality. See @DEBUGGER-POLICY-CONTROL.

  - The debugger was entered because of an interrupt such as `C-c`.

  - A hardware error such as a bus error occurred in code that was
    compiled unsafely due to the value of the SAFETY
    optimization quality."
  ;; FIXME: reinstate link when section on optimize qualities exists.
  ;; @OPTIMIZE-DECLARATION.
  "In the last two cases, the values of argument variables are
  accessible, but may be incorrect. For more details on when variable
  values are accessible, see @VARIABLE-VALUE-AVAILABILITY.

  It is possible for an interrupt to happen when a function call or
  return is in progress. The debugger may then flame out with some
  obscure error or insist that the bottom of the stack has been
  reached, when the real problem is that the current stack frame can't
  be located. If this happens, return from the interrupt and try
  again.")

(defsection @variable-access (:title "Variable Access")
  "There are two ways to access the current frame's local variables in
  the debugger: `list-locals` and SB-DEBUG:VAR.

  The debugger doesn't really understand lexical scoping; it has just
  one namespace for all the variables in the current stack frame. If a
  symbol is the name of multiple variables in the same function, then
  the reference appears ambiguous, even though lexical scoping
  specifies which value is visible at any given source location. If
  the scopes of the two variables are not nested, then the debugger
  can resolve the ambiguity by observing that only one variable is
  accessible.

  When there are ambiguous variables, the evaluator assigns each one a
  small integer identifier. The SB-DEBUG:VAR function uses this
  identifier to distinguish between ambiguous variables. The
  `list-locals` command prints the identifier. In the following
  example, there are two variables named `X`. The first one has
  identifier 0 (which is not printed), the second one has identifier
  1.

      X  =  1
      X#1  =  2

  - `list-locals [<prefix>]`: This command prints the name and value
    of all variables in the current frame whose name has the specified
    `<prefix>`, which may be a string or a symbol. If no `<prefix>` is
    given, then all available variables are printed. If a variable has
    a potentially ambiguous name, then the name is printed with a
    `#<identifier>` suffix, where `<identifier>` is the small integer
    used to make the name unique."
  (sb-debug:var function)
  (@variable-value-availability section)
  (@note-on-lexical-variable-access section))

(defsection @variable-value-availability (:title "Variable Value Availability")
  "The value of a variable may be unavailable to the debugger in portions
  of the program where Lisp says that the variable is defined. If a
  variable value is not available, the debugger will not let you read
  or write that variable. With one exception, the debugger will never
  display an incorrect value for a variable. Rather than displaying
  incorrect values, the debugger tells you the value is unavailable.

  The one exception is this: if you interrupt (e.g. with `C-c`) or if
  there is an unexpected hardware error such as a bus error (which
  should only happen in unsafe code), then the values displayed for
  arguments to the interrupted frame might be incorrect. This
  exception applies only to the interrupted frame: any frame farther
  down the stack will be fine.

  > _Note_: Since the location of an interrupt or hardware error will
  > always be an unknown location, non-argument variable values will
  > never be available in the interrupted frame. See
  > @UNKNOWN-LOCATIONS-AND-INTERRUPTS.)

  The value of a variable may be unavailable for these reasons:

  - The value of the DEBUG optimization quality may have omitted debug
    information needed to determine whether the variable is available.
    Unless a variable is an argument, its value will only be available
    when DEBUG is at least 2.

  - The compiler did lifetime analysis and determined that the value
    was no longer needed, even though its scope had not been exited.
    Lifetime analysis is inhibited when the DEBUG optimization
    quality is 3.

  - The variable's name is an uninterned symbol (gensym). To save
    space, the compiler only dumps debug information about uninterned
    variables when the DEBUG optimization quality is 3.

  - The frame's location is unknown (see
    @UNKNOWN-LOCATIONS-AND-INTERRUPTS) because the debugger was
    entered due to an interrupt or unexpected hardware error. Under
    these conditions the values of arguments will be available, but
    might be incorrect. This is the exception mentioned above.

  - The variable (or the code referencing it) was optimized out of
    existence. Variables with no reads are always optimized away. The
    degree to which the compiler deletes variables will depend on the
    value of the COMPILATION-SPEED optimization quality, but most
    source-level optimizations are done under all compilation
    policies.

  - The variable is never set and its definition looks like

          (LET ((var1 var2))
             ...)

      In this case, `VAR1` is substituted with `VAR2`.

  - The variable is never set and is referenced exactly once. In this
    case, the reference is substituted with the variable initial
    value.

  Since it is especially useful to be able to get the arguments to a
  function, argument variables are treated specially when the SPEED
  optimization quality is less than 3 and the DEBUG quality is at
  least 1. With this compilation policy, the values of argument
  variables are almost always available everywhere in the function,
  even at unknown locations. For non-argument variables, DEBUG must be
  at least 2 for values to be available, and even then, values are
  only available at known locations.")

(defsection @note-on-lexical-variable-access
    (:title "Note On Lexical Variable Access")
  "When the debugger command loop establishes variable bindings for
  available variables, these variable bindings have lexical scope and
  dynamic extent. You can close over them, but such closures can't be
  used as upward function arguments.

  > _Note_: The variable bindings are actually created using the Lisp
  > SYMBOL-MACROLET special form.

  You can also set local variables using SETQ, but if the variable was
  closed over in the original source and never set, then setting the
  variable in the debugger may not change the value in all the
  functions the variable is defined in. Another risk of setting
  variables is that you may assign a value of a type that the compiler
  proved the variable could never take on. This may result in bad
  things happening.")

(defsection @source-location-printing (:title "Source Location Printing")
  "One of the debugger's capabilities is source level debugging of
  compiled code.  These commands display the source location for the
  current frame:

  - `source [<context>]`: This command displays the file that the
    current frame's function was defined from (if it was defined from
    a file), and then the source form responsible for generating the
    code that the current frame was executing. If `<context>` is
    specified, then it is an integer specifying the number of
    enclosing levels of list structure to print.

  The source form for a location in the code is the innermost list
  present in the original source that encloses the form responsible
  for generating that code. If the actual source form is not a list,
  then some enclosing list will be printed. For example, if the source
  form was a reference to the variable `*SOME-RANDOM-SPECIAL*`, then
  the innermost enclosing evaluated form will be printed. Here are
  some possible enclosing forms:

      (let ((a *some-random-special*))
        ...)

      (+ *some-random-special* ...)

  If the code at a location was generated from the expansion of a
  macro or a source-level compiler optimization, then the form in the
  original source that expanded into that code will be printed.
  Suppose the file `/usr/me/mystuff.lisp` looked like this:

      (defmacro mymac ()
        '(myfun))

      (defun foo ()
        (mymac)
        ...)

  If `FOO` has called `MYFUN`, and is waiting for it to return, then
  the `source` command would print:

      ; File: /usr/me/mystuff.lisp

      (MYMAC)

  Note that the macro use was printed, not the actual function call form,
  `(MYFUN)`.

  If enclosing source is printed by giving an argument to `source` or
  `vsource`, then the actual source form is marked by wrapping it in a
  list whose first element is `#:***HERE***`. In the previous example,
  `source 1` would print:

      ; File: /usr/me/mystuff.lisp

      (DEFUN FOO ()
        (#:***HERE***
         (MYMAC))
        ...)"
  (@how-the-source-is-found section)
  (@source-location-availability section))

(defsection @how-the-source-is-found (:title "How the Source is Found")
  "If the code was defined from Lisp by COMPILE or EVAL, then the source
  can always be reliably located. If the code was defined from a FASL
  file created by COMPILE-FILE, then the debugger gets the source
  forms it prints by reading them from the original source file. This
  is a potential problem, since the source file might have moved or
  changed since the time it was compiled.

  The source file is opened using the TRUENAME of the source file
  pathname originally given to the compiler. This is an absolute
  pathname with all logical names and symbolic links expanded. If the
  file can't be located using this name, then the debugger gives up
  and signals an error.

  If the source file can be found, but has been modified since the time it was
  compiled, the debugger prints this warning:

      ; File has been modified since compilation:
      ;   <filename>
      ; Using form offset instead of character position.

  where `<filename>` is the name of the source file. It then proceeds
  using a robust but not foolproof heuristic for locating the source.
  This heuristic works if:

  - No top-level forms before the top-level form containing the source
    have been added or deleted, and

  - the top-level form containing the source has not been modified
    much. (More precisely, none of the list forms beginning before the
    source form have been added or deleted.)

  If the heuristic doesn't work, the displayed source will be wrong,
  but will probably be near the actual source. If the \"shape\" of the
  top-level form in the source file is too different from the original
  form, then an error will be signaled. When the heuristic is used,
  the source location commands are noticeably slowed.

  Source location printing can also be confused if (after the source
  was compiled) a read-macro you used in the code was redefined to
  expand into something different, or if a read-macro ever returns the
  same EQ list twice. If you don't define read macros and don't use
  `##` in perverted ways, you don't need to worry about this.")

(defsection @source-location-availability
    (:title "Source Location Availability")
  "Source location information is only available when the DEBUG
  optimization quality is at least 2. If source location information
  is unavailable, the source commands will give an error message.

  If source location information is available, but the source location
  is unknown because of an interrupt or unexpected hardware error
  (see @UNKNOWN-LOCATIONS-AND-INTERRUPTS), then the command will
  print

      Unknown location: using block start.

  and then proceed to print the source location for the start of the
  _basic block_ enclosing the code location. It's a bit complicated to
  explain exactly what a basic block is, but here are some properties
  of the block start location:

  - The block start location may be the same as the true location.

  - The block start location will never be later in the program's flow
    of control than the true location.

  - No conditional control structures (such as IF, COND, OR) will
    intervene between the block start and the true location (but note
    that some conditionals present in the original source could be
    optimized away.) Function calls _do not_ end basic blocks.

  - The head of a loop will be the start of a block.

  - The programming language concept of block structure and the Lisp
    BLOCK special form are totally unrelated to the compiler's basic
    block.

  In other words, the true location lies between the printed location
  and the next conditional (but watch out because the compiler may
  have changed the program on you.)")

(defsection @debugger-policy-control (:title "Debugger Policy Control")
  "The compilation policy specified by OPTIMIZE declarations
  affects the behavior seen in the debugger. The DEBUG quality
  directly affects the debugger by controlling the amount of debugger
  information dumped. Other optimization qualities have indirect but
  observable effects due to changes in the way compilation is done.

  Unlike the other optimization qualities (which are compared in
  relative value to evaluate tradeoffs), the DEBUG optimization
  quality is directly translated to a level of debug information. This
  absolute interpretation allows the user to count on a particular
  amount of debug information being available even when the values of
  the other qualities are changed during compilation. These are the
  levels of debug information that correspond to the values of the
  DEBUG quality:

  - `0`: Only the function name and enough information to allow the
    stack to be parsed.

  - `> 0`: Any level greater than 0 gives level 0 plus all argument
    variables. Values will only be accessible if the argument variable
    is never set and SPEED is not 3. SBCL allows any real value for
    optimization qualities. It may be useful to specify 0.5 to get
    backtrace argument display without argument documentation.

  - `1`: Level 1 provides argument documentation (printed argument
    lists) and derived argument/result type information. This makes
    DESCRIBE more informative, and allows the compiler to do
    compile-time argument count and type checking for any calls
    compiled at run-time. This is the default.

  - `2`: Level 1 plus all interned local variables, source location
    information, and lifetime information that tells the debugger when
    arguments are available (even when SPEED is 3 or the argument is
    set).

  - `> 2`: Any level greater than 2 gives level 2 and in addition
    disables tail-call optimization, so that the backtrace will
    contain frames for all invoked functions, even those in tail
    positions.

  - `3`: Level 2 plus all uninterned variables. In addition, lifetime
    analysis is disabled (even when SPEED is 3), ensuring that all
    variable values are available at any known location within the
    scope of the binding. This has a speed penalty in addition to the
    obvious space penalty.

  Inlining of local functions is inhibited so that they may be TRACEd.

  - `> (MAX SPEED SPACE)`: If DEBUG is greater than both SPEED and
    SPACE, the command `return` can be used to continue execution by
    returning a value from the current stack frame.

  - `> (MAX SPEED SPACE COMPILATION-SPEED)`: If DEBUG is greater than
    all of SPEED, SPACE and COMPILATION-SPEED the code will be
    steppable (see @SINGLE-STEPPING).

  As you can see, if the SPEED quality is 3, debugger performance is
  degraded. This effect comes from the elimination of argument
  variable special-casing (see @VARIABLE-VALUE-AVAILABILITY). Some
  degree of speed/debuggability tradeoff is unavoidable, but the
  effect is not too drastic when DEBUG is at least 2.

  In addition to INLINE and NOTINLINE declarations, the relative
  values of the SPEED and SPACE qualities also change whether
  functions are inline expanded. If a function is inline expanded,
  then there will be no frame to represent the call, and the arguments
  will be treated like any other local variable. Functions may also be
  _semi-inline_, in which case there is a frame to represent the call,
  but the call is to an optimized local version of the function, not
  to the original function."
  ;; FIXME: link to section about inline expansion when it exists
  ;; (@INLINE-EXPANSION).
  )

(defsection @exiting-commands (:title "Exiting Commands")
  "These commands get you out of the debugger.

  - `toplevel`: Throw to top level.

  - `restart [<n>]`: Invoke the `<n>`th restart case as displayed by
    the `error` command. If `<n>` is not specified, the available
    restart cases are reported.

  - `\\continue`: Call CONTINUE on the condition given to DEBUG. If
    there is no restart case named CONTINUE, then an error is
    signaled.

  - `\\abort`: Call ABORT on the condition given to DEBUG. This is
    useful for popping debug command loop levels or aborting to top
    level, as the case may be.

  - `return <value>`: Return `VALUE` from the current stack frame.
    This command is available when the DEBUG optimization quality is
    greater than both SPEED and SPACE. Care must be taken that the
    value is of the same type as SBCL expects the stack frame to
    return.

  - `restart-frame`: Restart execution of the current stack frame.
    This command is available when the DEBUG optimization quality is
    greater than both SPEED and SPACE and when the frame is for a
    global function. If the function is redefined in the debugger
    before the frame is restarted, the new function will be used.")

(defsection @information-commands (:title "Information Commands")
  "Most of these commands print information about the current frame or
  function, but a few show general information.

  - `help` or `?`: Display a synopsis of debugger commands.

  - `\\describe`: Call DESCRIBE on the current function and displays the
    number of local variables.

  - `\\print`: Display the current function call as it would be
    displayed by moving to this frame.

  - `\\error`: Print the condition given to INVOKE-DEBUGGER and the
    active proceed cases.

  - `backtrace [<n>]`: Display all the frames from the current to the
    bottom. Only shows `<n>` frames if specified. The printing is
    controlled by SB-DEBUG:*DEBUG-PRINT-VARIABLE-ALIST*.")

(defsection @breakpoint-commands (:title "Breakpoint Commands")
  "SBCL supports setting of breakpoints inside compiled functions and
  stepping of compiled code. Breakpoints can only be set at known
  locations (see @UNKNOWN-LOCATIONS-AND-INTERRUPTS), so these commands
  are largely useless unless the DEBUG optimize quality is at least
  2 (see @DEBUGGER-POLICY-CONTROL). These commands manipulate
  breakpoints:

  - `breakpoint <location> [<option> <value>]*`: Set a breakpoint in
    some function. `<location>` may be an integer code location
    number (as displayed by `list-locations`) or a keyword. The
    keyword can be used to indicate setting a breakpoint at the
    function start (:START, `:S`) or function end (:END, `:E`). The
    `breakpoint` command has :CONDITION, :BREAK, :PRINT and :FUNCTION
    options which work similarly to the TRACE options.

  - `list-locations [<function>]` or `ll [<function>]`: List all the
    code locations in the current frame's function, or in `<function>`
    if it is supplied. The display format is the code location number,
    a colon and then the source form for that location:

          3: (1- N)

      If consecutive locations have the same source, then a numeric
      range like `3-5:` will be printed. For example, a default
      function call has a known location both immediately before and
      after the call, which would result in two code locations with
      the same source. The listed function becomes the new default
      function for breakpoint setting (via the `breakpoint`) command.

  - `list-breakpoints` or `lb`: List all currently active breakpoints
    with their breakpoint number.

  - `delete-breakpoint [<number>]` or `db [<number>]`: Delete a
    breakpoint specified by its breakpoint number. If no number is
    specified, delete all breakpoints.

  - `step*`: Step to the next possible breakpoint location in the
    current function. This always steps over function calls, instead
    of stepping into them."
  (@breakpoint-example section))

(defsection @breakpoint-example (:title "Breakpoint Example")
  "Consider this definition of the factorial function:

      (defun ! (n)
        (if (zerop n)
            1
            (* n (! (1- n)))))

  This debugger session demonstrates the use of breakpoints:

      * (break)  ; invoke debugger

      debugger invoked on a SIMPLE-CONDITION in thread 11184: break

      restarts (invokable by number or by possibly-abbreviated name):
        0: [CONTINUE] Return from BREAK.
        1: [ABORT   ] Reduce debugger level (leaving debugger, returning to toplevel).
        2: [TOPLEVEL] Restart at toplevel READ/EVAL/PRINT loop.
      (\"varargs entry for top level local call BREAK\" \"break\")
      0] ll #'!

      0-1: (SB-INT:NAMED-LAMBDA ! (N) (BLOCK ! (IF (ZEROP N) 1 (* N (! #)))))
      2: (BLOCK ! (IF (ZEROP N) 1 (* N (! (1- N)))))
      3: (ZEROP N)
      4: (* N (! (1- N)))
      5: (1- N)
      6: (! (1- N))
      7-8: (* N (! (1- N)))
      9-10: (IF (ZEROP N) 1 (* N (! (1- N))))
      0] br 4

      (* N (! (1- N)))
      1: 4 in !
      added
      0] toplevel

      > (! 10) ; Call the function

      *Breakpoint hit*

      Restarts:
        0: [CONTINUE] Return from BREAK.
        1: [ABORT   ] Return to Top-Level.

      Debug  (type H for help)

      (! 10) ; We are now in first call (arg 10) before the multiply
      Source: (* N (! (1- N)))
      3] step*

      *Step*

      (! 10) ; We have finished evaluation of (1- n)
      Source: (1- N)
      3] step*

      *Breakpoint hit*

      Restarts:
        0: [CONTINUE] Return from BREAK.
        1: [ABORT   ] Return to Top-Level.

      Debug  (type H for help)

      (! 9) ; We hit the breakpoint in the recursive call
      Source: (* N (! (1- N)))
      3]

  > _Note_: The `step*` command differs from the single stepping
  > commands in that it also functions in compiled code which has not
  > been compiled with stepping instrumentation. It simply steps to
  > the next compiled code location. In the future, this form of
  > stepping may be improved enough to subsume the instrumentation
  > based stepping commands, which have much higher overhead.")

(defsection @function-tracing (:title "Function Tracing")
  "The tracer causes selected functions to print their arguments and
  their results whenever they are called.  Options allow conditional
  printing of the trace information and conditional breakpoints on
  function entry or exit.

  In SBCL, tracing can be done either by temporarily redefining the
  function name (encapsulation), or using breakpoints. When
  breakpoints are used, the function object itself is destructively
  modified to cause the tracing action. The advantage of using
  breakpoints is that tracing works even when the function is
  anonymously called via FUNCALL, that function object identity is
  preserved, and that anonymous and local functions can also be
  traced."
  (trace macro)
  "In the case of functions where the known return convention is used
  to optimize, encapsulation may be necessary in order to make tracing
  work at all. The symptom of this occurring is an error stating

      Error in function FOO: :FUNCTION-END breakpoints are
      currently unsupported for the known return convention.

  in such cases we recommend using `(TRACE FOO :ENCAPSULATE t)`."
  (untrace macro)
  (sb-debug:*trace-indentation-step* variable)
  (sb-debug:*max-trace-indentation* variable)
  (sb-debug:*trace-encapsulate-default* variable)
  (sb-debug:*trace-report-default* variable))

(defsection @single-stepping (:title "Single Stepping")
  "SBCL includes an instrumentation based single-stepper for compiled
  code, that can be invoked via the STEP macro, or from within the
  debugger. See @DEBUGGER-POLICY-CONTROL, for details on enabling
  stepping for compiled code.

  The following debugger commands are used for controlling single stepping.

  - `start`: Select the CONTINUE restart if one exists and starts
    single stepping. None of the other single stepping commands can be
    used before stepping has been started either by using `start` or
    by using the standard STEP macro.

  - `step`: Step into the current form. Stepping will be resumed when
    the next form that has been compiled with stepper instrumentation
    is evaluated.

  - `next`: Step over the current form. Stepping will be disabled
    until evaluation of the form is complete.

  - `out`: Step out of the current frame. Stepping will be disabled
    until the topmost stack frame that had been stepped into returns.

  - `stop`: Stop the single stepper and resumes normal execution."
  (step macro))

(defsection @enabling-and-disabling-the-debugger
    (:title "Enabling and Disabling the Debugger")
  "In certain contexts (e.g. non-interactive applications), it may be
  desirable to turn off the SBCL debugger (and possibly re-enable it).
  The functions here control the debugger."
  (sb-ext:disable-debugger function)
  (sb-ext:enable-debugger function))
