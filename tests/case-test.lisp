(macrolet ((mcase (&rest cases)
             `(case 10
                ,@(loop for case in cases
                        collect (list case 20)))))
  (mcase 10 10))
