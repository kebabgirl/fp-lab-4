(defun bubble-step (lst limit last-swap &key (key #'identity) (test #'<))
  (if (or (null (cdr lst)) (= limit 0))
      (values lst last-swap)
      (let* ((a (car lst))
             (b (cadr lst))
             (key-a (funcall key a))
             (key-b (funcall key b)))
        (if (funcall test key-b key-a)
            (let* ((swapped (cons b (cons a (cddr lst)))))
              (multiple-value-bind (sorted-lst new-last-swap)
                  (bubble-step (cdr swapped) (1- limit) t :key key :test test)
                (values (cons (car swapped) sorted-lst)
                        (or new-last-swap (1- limit)))))
            (multiple-value-bind (sorted-lst new-last-swap)
                (bubble-step (cdr lst) (1- limit) last-swap :key key :test test)
              (values (cons a sorted-lst)
                      (or new-last-swap last-swap)))))))

(defun recursive-bubble-sort (lst &key (key #'identity) (test #'<))
  (labels ((recursive-sort (lst limit)
             (multiple-value-bind (new-lst last-swap)
                 (bubble-step lst limit nil :key key :test test)
               (if last-swap
                   (recursive-sort new-lst (1- limit))
                   new-lst))))
    (recursive-sort lst (length lst))))

(defun check-recursive-sort (test-name input expected &key (key #'identity) (test #'<))
  (let ((result (recursive-bubble-sort input :key key :test test)))
    (if (equal result expected)
        (format t "~a passed: ~a -> ~a~%" test-name input result)
        (format t "~a failed: ~a -> ~a (expected: ~a)~%" test-name input result expected))))

(defun test-recursive-sort ()
  (check-recursive-sort "Test 1" '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9))
  (check-recursive-sort "Test 2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-recursive-sort "Test 3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-recursive-sort "Test 4" '() '())
  (check-recursive-sort "Test 5" '(1) '(1))
  (check-recursive-sort "Test 6" '(1 2 2 1) '(1 1 2 2))

  (check-recursive-sort "Test 7" '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(-1 1 -2 3 3 -4 -5 -5 -5 6 9) :key #'abs)
  (check-recursive-sort "Test 8" '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(-1 1 -2 3 3 -4 -5 -5 -5 6 9) :key #'abs :test #'<)
  (check-recursive-sort "Test 9" '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(1 -1 -2 3 3 -4 -5 -5 -5 6 9) :key #'abs :test #'<=)

  (check-recursive-sort "Test 10" '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(9 6 -5 -5 -5 -4 3 3 -2 -1 1) :key #'abs :test #'>)
  (check-recursive-sort "Test 11" '(3 2 5 4 1) '(1 2 3 4 5) :test #'<=)
  (check-recursive-sort "Test 12" '(3 2 5 4 1) '(5 4 3 2 1) :test #'>))

(defun remove-each-nth-fn (n &key key)
  (let ((count 0))
    (lambda (element)
      (setf count (+ count 1))
      (if key
          (and (= (mod count n) 0) (funcall key element))
          (= (mod count n) 0)))))

(defun check-remove-each-nth-fn (test-name input n expected &key key)
  (let ((result (remove-if (remove-each-nth-fn n :key key) input)))
    (if (equal result expected)
        (format t "~a passed: ~a -> ~a~%" test-name input result)
        (format t "~a failed: ~a -> ~a (expected: ~a)~%" test-name input result expected))))

(defun test-remove-each-nth-fn ()
  (check-remove-each-nth-fn "Test 1" '(1 2 3 4 5) 2 '(1 3 5))
  (check-remove-each-nth-fn "Test 2" '(1 2 3 4 5 6) 3 '(1 2 4 5))
  (check-remove-each-nth-fn "Test 3" '(1 2 3 4) 1 '())

  (check-remove-each-nth-fn "Test 4" '(1 2 3 4) 2 '(1 3) :key #'evenp)
  (check-remove-each-nth-fn "Test 5" '(1 2 2 2 3 3 4 4 5) 3 '(1 2 2 2 3 4 4) :key #'oddp)
  (check-remove-each-nth-fn "Test 6" '(1 2 2 2 2 3 4 4 4 5) 2 '(1 2 2 3 4 4 5) :key #'evenp))







