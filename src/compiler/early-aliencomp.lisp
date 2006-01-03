(in-package "SB!C")

(defknown %alien-funcall (system-area-pointer alien-type &rest *) *)
(defknown %alien-funcall-stdcall (system-area-pointer alien-type &rest *) *)
