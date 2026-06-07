(in-package :sb-manual)

(defsection @sb-aclrepl (:title "sb-aclrepl")
  "The `SB-ACLREPL` module offers an Allegro CL-style
  Read-Eval-Print Loop for SBCL, with integrated inspector. Adding a
  debugger interface is planned.

  Allegro CL is a registered trademark of Franz Inc."
  (@sb-aclrepl-usage section)
  (@sb-aclrepl-customization section)
  (@sb-aclrepl-example-initialization section))

(defsection @sb-aclrepl-usage (:title "Usage")
  "To start `SB-ACLREPL` as your read-eval-print loop, put the form

      (require 'sb-aclrepl)

  in your `~/.sbclrc`, one of your @INITIALIZATION-FILES.")

(defsection @sb-aclrepl-customization (:title "Customization")
  "The following customization variables are available:"
  (sb-aclrepl:*command-char* variable)
  (sb-aclrepl:*prompt* variable)
  (sb-aclrepl:*exit-on-eof* variable)
  (sb-aclrepl:*use-short-package-name* variable)
  (sb-aclrepl:*max-history* variable))

(defsection @sb-aclrepl-example-initialization (:title "Example Initialization")
  "Here's a longer example of a `~/.sbclrc` file that shows off
  some of the features of sb-aclrepl:

      (ignore-errors (require 'sb-aclrepl))

      (when (find-package 'sb-aclrepl)
        (push :aclrepl cl:*features*))
      #+aclrepl
      (progn
        (setq sb-aclrepl:*max-history* 100)
        (setf (sb-aclrepl:alias \"asdc\")
             #'(lambda (sys) (asdf:operate 'asdf:compile-op sys)))
        (sb-aclrepl:alias \"l\" (sys) (asdf:operate 'asdf:load-op sys))
        (sb-aclrepl:alias \"t\" (sys) (asdf:operate 'asdf:test-op sys))
        ;; The 1 below means that two characaters (\"up\") are required
        (sb-aclrepl:alias (\"up\" 1 \"Use package\") (package) (use-package package))
        ;; The 0 below means only the first letter (\"r\") is required,
        ;; such as \":r base64\"
        (sb-aclrepl:alias (\"require\" 0 \"Require module\") (sys) (require sys))
        (setq cl:*features* (delete :aclrepl cl:*features*)))

  Questions, comments, or bug reports should be sent to Kevin Rosenberg
  (kevin@rosenberg.net).")
