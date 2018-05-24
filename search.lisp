(load "util.lisp")
(load "pat.lisp")
(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p. Start with states,
   and search according to successors and combiner."
  (dbg :search "~&;; Search: ~a" states)
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
(defun is (value &key (key #'identity) (test #'eql))
  "Returns a predicate that tests for a given value."
  #'(lambda (x) (funcall test value (funcall key x))))

(defun path-saver (successors cost-fn cost-left-fn)
  #'(lambda (old-path)
      (let ((old-state (path-state old-path)))
        (mapcar
          #'(lambda (new-state)
              (let ((old-cost
                      (+ (path-cost-so-far old-path)
                         (funcall cost-fn old-state new-state))))
                (make-path
                  :state new-state
                  :previous old-path
                  :cost-so-far old-cost
                  :total-cost (+ old-cost (funcall cost-left-fn
                                                   new-state)))))
          (funcall successors old-state)))))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
          (path-state path) (path-total-cost path)))

(defun show-city-path (path &optional (stream t))
  "Show length of a path, and the cities along it."
  (format stream "#<Path ~,1f km: ~{~:(~a~)~^ - ~}>"
          (path-total-cost path)
          (reverse (map-path #'city-name path)))
  (values))

(defun map-path (fn path)
  "Call fn on each state in the path collecting results."
  (if (null path)
      nil
      (cons (funcall fn (path-state path))
            (map-path fn (path-previous path)))))

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

(defstruct (city (:type list)) name long lat)
(defparameter *cities*
  '((Atlanta 84.23 33.45) (Los-Angeles 118.15 34.03)
    (Boston 71.05 42.21) (Memphis 90.03 35.09)
    (Chicago 87.37 41.50) (New-York 73.58 40.47)
    (Denver 105.00 39.45) (Oklahoma-City 97.28 35.26)
    (Eugene 123.05 44.03) (Pittsburgh 79.57 40.27)
    (Flagstaff 111.41 35.13) (Quebec 71.11 46.49)
    (Grand-Jct 108.37 39.05) (Reno 119.49 39.30)
    (Houston 105.00 34.00) (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46) (Tampa 82.27 27.57)
    (Jacksonville 81.40 30.22) (Victoria 123.21 48.25)
    (Kansas-City 94.35 39.06) (Wilmington 77.57 34.14)))

(defun neighbors (city)
  "Find all cities with 1000 kilometers."
  (find-all-if #'(lambda (c)
                   (and (not (eq c city))
                        (< (air-distance c city) 1000.0)))
               *cities*))

(defun city (name)
  "Find the city with this name."
  (assoc name *cities*))

(defconstant earth-diameter 12765.0
  "Diameter of planet Earth in kilometers.")

(defun air-distance (a b)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords a) (xyz-coords b))))
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
   The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Eucledian distance between two points.
   The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

(defun deg->radians (deg)
  "Convert degrees and minutes into radians."
  (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))

(defun trip (start dest &optional (beam-width 1))
  "Search for the best path from start to dest."
  (beam-search
    (make-path :state start)
    (is dest :key #'path-state)
    (path-saver #'neighbors #'air-distance
                #'(lambda (c) (air-distance c dest)))
    #'path-total-cost
    beam-width))

(defstruct (path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))

(defun graph-search (states goal-p successors combiner
                     &optional (state= #'eql) old-states)
  "Find a state that satisfies goal-p. Start with states,
   and search according to successors and combiner.
   Don't try the same state twice."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search
             (funcall
               combiner
               (new-states states successors state= old-states)
               (rest states))
             goal-p successors combiner state=
             (adjoin (first states) old-states
                     :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before."
  (remove-if
    #'(lambda (state)
        (or (member state states :test state=)
            (member state old-states :test state=)))
    (funcall successors (first states))))

(defun next2 (x) (list (+ x 1) (+ x 2)))

(defun find-path (state paths state=)
  "Find the path with this state among a list of paths."
  (find state paths :key #'path-state :test state=))

(defun better-path (path1 path2)
  "Is path1 cheaper than path2?"
  (< (path-total-cost path1) (path-total-cost path2)))

(defun insert-path (path paths)
  "Put path into the right position, sorted by total cost."
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun path-states (path)
  "Collect the states along this path."
  (if (null path)
      nil
      (cons (path-state path)
            (path-states (path-previous path)))))

(defun a*-search (paths goal-p successors cost-fn cost-left-fn
                        &optional (state= #'eql) old-paths)
  "Find a path whose state satisfies goal-p. Start with paths,
   and expand successors, exporing least cost first.
   When there are duplicate states, keep the one with the
   lower cost and discard the other."
  (dbg :search ";; Search: ~a" paths)
  (cond
    ((null paths) fail)
    ((funcall goal-p (path-state (first paths)))
     (values (first paths) paths))
    (t (let* ((path (pop paths))
              (state (path-state path)))
         ;; Update paths and old-paths to reflect
         ;; the new successors of state
         (setf old-paths (insert-path path old-paths))
         (dolist (state2 (funcall successors state))
           (let* ((cost (+ (path-cost-so-far path)
                           (funcall cost-fn state state2)))
                  (cost2 (funcall cost-left-fn state2))
                  (path2 (make-path
                           :state state2 :previous path
                           :cost-so-far cost
                           :total-cost (+ cost cost2)))
                  (old nil))
             (cond
               ((setf old (find-path state2 paths state=))
                (when (better-path path2 old)
                  (setf paths
                        (insert-path path2 (delete old paths)))))
               ((setf old (find-path state2 old-paths state=))
                (when (better-path path2 old)
                  (setf paths (insert-path path2 paths))
                  (setf old-paths (delete old old-paths))))
               (t (setf paths (insert-path path2 paths))))))
         (a*-search paths goal-p successors cost-fn cost-left-fn
                    state= old-paths)))))

(defun search-all (start goal-p successors cost-fn beam-width)
  "Find all solutions to a search problem, using beam search."
  (let ((solutions nil))
    (beam-search
      start #'(lambda (x)
                (when (funcall goal-p x) (push x solutions))
                nil)
      successors cost-fn beam-width)
    solutions))
