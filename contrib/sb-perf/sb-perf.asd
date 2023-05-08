;;;; SB-Perf

(error "Can't build contribs with ASDF")

(defsystem "sb-perf"
  :description "Symbol export for Perf analysis"
  :author "Philipp Marek <philipp@marek.priv.at>"
  :licence "BSD"
  :version "1.0.0"
  :components ((:file "export-syms")))

