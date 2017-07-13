(defconstant +box-index-list+
  (loop for n in '(0 3 6)
     append (loop repeat 3
               append (loop repeat 3 
                         for i from n
                         append (loop repeat 3 collect i)))))

(defconstant +all-digits+ '(1 2 3 4 5 6 7 8 9))

(define-condition impossible () ())

;;; from Common Lisp Recipes
(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array (make-array dims
                              :element-type (array-element-type array)
                              :displaced-to array)
                  dims)))

(defun box-index-of (idx)
  (nth idx +box-index-list+))
    
(defun unknown-cells (pz)
  (loop repeat (array-total-size pz)
     for idx from 0
     for cell = (row-major-aref pz idx)
     if (= cell 0) collect (list (floor idx 9) (mod idx 9) (box-index-of idx))))

(defun remove-zero (set)
  (set-difference set '(0)))

(defun row-digits (pz row)
  (remove-zero (mapcar #'(lambda (x) (aref pz row x))
                       '(0 1 2 3 4 5 6 7 8))))

(defun column-digits (pz column)
  (remove-zero (mapcar #'(lambda (x) (aref pz x column))
                       '(0 1 2 3 4 5 6 7 8))))

(defun box-digits (pz box)
  (remove-zero (mapcar #'(lambda (x) (row-major-aref pz x))
                       (loop for digit in +box-index-list+
                          for row-major-index from 0
                          if (= digit box) collect row-major-index))))

(defun possible (pz row column box)
  (set-difference +all-digits+
                  (remove-duplicates (append (row-digits pz row)
                                             (column-digits pz column)
                                             (box-digits pz box)))))

(defun has-duplicates-p (pz row column)
  (flet ((duplicates-p (lst)
           (not (equal (remove-duplicates lst) lst))))
    (or (duplicates-p (row-digits pz row))
        (duplicates-p (column-digits pz column))
        (duplicates-p (box-digits pz (box-index-of (array-row-major-index pz row column)))))))                                                                 
                                  

(defun scan (pz)
  (loop named outer for cells = (unknown-cells pz)
     do
       (loop for (r c b) in cells
          for digit-list = (possible pz r c b)
          if (null digit-list) do               ;NG case
            (signal 'impossible)
          else if (= (length digit-list) 1) do
            (setf (aref pz r c) (car digit-list))
          and count r into update
          else minimize (length digit-list) into min
          and collect (list r c digit-list) into not-determined
          finally
            (if (zerop update)
                (return-from outer
                  (values (find min not-determined
                                :key #'(lambda (x) (length (third x))))
                          (= min 0)))))))

(defun solve (puzzle)
  (let ((pz (copy-array puzzle)))
    (multiple-value-bind (possible finish) (scan pz)
      (if finish
          pz
          (destructuring-bind (r c guess-list) possible
            (loop for guess in guess-list
               do
                 (setf (aref pz r c) guess)
                 (handler-case
                     (return (solve pz))
                   (impossible ()))
               finally
                 (signal 'impossible)))))))

(defun sudoku (lst)
  (let ((puzzle (make-array '(9 9) :initial-contents lst)))
    (loop repeat (array-total-size puzzle)
       for idx from 0
       for cell = (row-major-aref puzzle idx)
       do
         (if (or (not (numberp cell)) (< cell 0) (> cell 9)
                 (has-duplicates-p puzzle (floor idx 9) (mod idx 9)))
             (error "invalid data")))
    (solve puzzle)))
