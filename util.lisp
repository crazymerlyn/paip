(defun find-all (item sequence &rest keyword-args
                  &key (test #'eq) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
   according to keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))
