(in-package "SB!KERNEL")

;; Not sure why this needs to be 'late'. A deftype body produces forms,
;; not code that cares whether sb!eval:interpreted-function means anything.
(sb!xc:deftype compiled-function ()
  '(and function
        #!+sb-fasteval (not sb!interpreter:interpreted-function)
        #!+sb-eval (not sb!eval:interpreted-function)))

#+sb-xc-host
;; Restore this to its build-the-xc compile-time value at load-time.
(setq sb!impl::!*xc-processed-deftypes* '#.sb!impl::!*xc-processed-deftypes*)
