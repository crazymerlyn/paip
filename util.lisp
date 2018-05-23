(defun find-all (item sequence &rest keyword-args
                  &key (test #'eq) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
   according to keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun find-all-if (predicate sequence)
  "Find all those elements of sequence that match predicate.
   Doesn't alter sequence."
  (remove-if #'(lambda (item) (not (funcall predicate item))) sequence))

(defun starts-with (seq elem)
  "Determine whether `seq`  starts with elem"
  (and (consp seq) (eq (first seq) elem)))

(defvar *dbg-ids* nil "Identifiers used by dbg.")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun add-debug (&rest ids)
  "Start dbg output on given ids."
  (setf *dbg-ids* (union *dbg-ids* ids)))

(defun undebug (&rest ids)
  "Stop dbg output on given ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))
