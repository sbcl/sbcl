(in-package "SB!C")

(!begin-collecting-cold-init-forms)

(!cold-init-forms
 (map 'nil
      (lambda (saetp)
	(setf (sb!vm:saetp-ctype saetp)
	      (specifier-type (sb!vm:saetp-specifier saetp))))
      sb!vm:*specialized-array-element-type-properties*))

(!cold-init-forms
 (maphash
  (lambda (key value)
    (declare (ignore key))
    (setf (primitive-type-type value)
	  (specifier-type (type-specifier (primitive-type-type value)))))
  *backend-meta-primitive-type-names*))

(!cold-init-forms
 (maphash
  (lambda (key value)
    (declare (ignore key))
    (setf (primitive-type-type value)
	  (specifier-type (type-specifier (primitive-type-type value)))))
  *backend-primitive-type-names*))

(!defun-from-collected-cold-init-forms !fixup-type-cold-init)