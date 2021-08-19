(eval-when (:compile-toplevel :load-toplevel)
  (sb-ext:unlock-package :sb-c))

(defpackage :sb-graph
  (:shadow :stream)
  (:use :cl :cl-user)
  (:export :hook :disable-hook :enable-hook :unhook :hook-enabled
   :make-graph :make-and-dfs :save-graph :graph :render-graph
   :expand :expand-codename :get-node-from-codename :expand :unexpand :get-node
   :interactively-graph :output :dfs-add))
