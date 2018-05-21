(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p. Start with states,
   and search according to successors and combiner."
  (print states)
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
             (funcall combiner
                      (funcall successors (first states))
                      (rest states))
             goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

(defun prepend (x y) "Prepend y to start of x" (append y x))

(defun breadth-first-search (start goal-p successors)
  "Search for old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))
(defun is (value) #'(lambda (x) (eql x value)))

(defun finite-binary-tree (n)
  "Return a successor function that generates a binary tree
   with n nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
                 (binary-tree x))))

(defun diff (num)
  "Return the function that finds the difference from num."
  #'(lambda (x) (abs (- num x))))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun price-is-right (price)
  "Return a function that measures the difference from price,
   but gives a big penalty for going over price."
  #'(lambda (x) (if (> x price)
                    most-positive-fixnum
                    (- price x))))

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reached,
   but never consider more than beam-width states at a time."
  (tree-search (list start) goal-p successors
               #'(lambda (new old)
                   (let ((sorted (funcall (sorter cost-fn) new old)))
                     (if (> beam-width (length sorted))
                         sorted
                         (subseq sorted 0 beam-width))))))
