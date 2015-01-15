(labels ((a (x)))
  (error 'program-error)
  (a (catch 'c)))
