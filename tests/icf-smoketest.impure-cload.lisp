(eval '(defun hhh () 92))
(defvar f #'hhh)
(unintern 'hhh)
(eval '(defun hhh () 92))
;;; Ensure no crash on (FIND-PACKAGE (SYMBOL-PACKAGE NIL)) if two functions
;;; are identical and one is named by an uninterned symbol.
(fold-identical-code :aggressive t)
