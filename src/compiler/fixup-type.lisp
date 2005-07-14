(in-package "SB!C")

(!begin-collecting-cold-init-forms)

(!cold-init-forms
 (map 'nil
      (lambda (saetp)
        (setf (sb!vm:saetp-ctype saetp)
              (specifier-type (sb!vm:saetp-specifier saetp))))
      sb!vm:*specialized-array-element-type-properties*))

(!defun-from-collected-cold-init-forms !fixup-type-cold-init)