;;; package --- Tyck test suite -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'tyck)

(defun tyck-should-type (type expr)
  "Test that TYPE is the inferred type of EXPR."
  (should (equal type (car (tyck-type-of expr)))))

(defmacro tyck-type-test (name &rest body)
  "Define an ERT test NAME.

Expect TYPE to be equal to the inferred type of EXPR."
  (declare (indent 1))
  `(ert-deftest ,name ()
     ,@(mapcar (lambda (b)
                 (pcase b
                   (`(ok ,type ,expr)
                    `(tyck-should-type ',type ',expr))
                   (`(err ,expr)
                    `(should-error (tyck-type-of ',expr)
                                   :type 'type-error))
                   (_ (error "Invalid clause %S" b))))
               body)))

(tyck-type-test basic-types
  (ok number (+ 1 1 1 1 1))
  (err (+ 1 "a"))
  (ok list (list 1 2 3))
  (ok list (list))
  (err (cdr))
  (err (cdr (list) (list)))
  (ok list (cdr (list 1 "a" 3)))
  (ok bool (< 1 2 3))
  (ok bool (< 1))
  (err (<))
  (ok number (1+ 3))
  (err (1+ "a")))

(tyck-type-test loop
  (ok list (let ((i 0)
                 (num 2)
                 (list (list 1 2 3)))
             (while (and (< i num) list)
               (setq list (cdr list))
               (setq i (1+ i)))
             list)))

(tyck-type-test if
  (ok (string list) (if 0 "a" (list))))

(tyck-type-test macro
  (ok (number unit) (when t 1)))

(provide 'tyck-test)
;;; tyck-test.el ends here
