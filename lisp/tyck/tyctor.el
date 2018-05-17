;;; package --- Tyctor type inspector -*- lexical-binding: t; -*-
;;; Function used to collect the values of Flycheck functions at runtime, and
;;; infer they types.

(defvar fun-to-values (make-hash-table))

(defun instrument-flycheck-code ()
  "Collect arguments and return values of every flycheck function."
  (mapatoms
   (lambda (sym)
     (let ((name (symbol-name sym)))
       (when (and (functionp sym)
                  (string-prefix-p "flycheck-" name)
                  (not (string-prefix-p "flycheck-ert-" name))
                  ;; This one is called too often
                  (not (string= "flycheck-valid-checker-p" name)))
         (advice-add sym :around
                     (lambda (orig-fun &rest args)
                       (let ((out (apply orig-fun args)))
                         (cl-pushnew (cons args out)
                                     (gethash sym fun-to-values)
                                     :test #'equal)
                         out))))))))

(defun infer-type-single (value)
  "Infer type of a single VALUE."
  (cond ((eq nil value) 'null)
        ((eq t value) 'true)
        ((and (consp value) (not (listp (cdr value))))
         (format "cons(%s %s)"
                 (infer-type-single (car value))
                 (infer-type-single (cdr value))))
        ((and (vectorp value) (> (length value) 0)
              (string-match "cl-struct-\\([a-z-]+\\)" (symbol-name (elt value 0))))
         (format "struct(%s)" (match-string 1 (symbol-name (elt value 0)))))
        (t (type-of value))))

(defun infer-type (values)
  "Infer type of VALUES for a function call."
  (pcase-let ((`(,args . ,out) values))
    (cons (mapcar #'infer-type-single args) (infer-type-single out))))

(defun merge-type-single (left right)
  "Merge types LEFT and RIGHT."
  (cond ((equal left right) left)
        ((and (listp left) (listp right)) (-union left right))
        ((and (listp left) (not (null left))) (-union left (list right)))
        ((listp left) right)
        ((and (listp right) (not (null right))) (-union right (list left)))
        ((listp right) left)
        ;; FIXME: this does not catch the case where 'true and 'null are added
        ;; to an OR one after the other.  We should rather do a simplification
        ;; step on full types (e.g., flatten OR and merge 'true and 'null)
        ((or (and (eq 'null left) (eq 'true right))
             (and (eq 'true left) (eq 'null right)))
         'boolean)
        (t (list 'OR left right))))

(defun merge-types (left right)
  "Merge type signatures LEFT and RIGHT."
  (pcase-let* ((`(,args1 . ,out1) left)
              (`(,args2 . ,out2) right)
              (`(,args1 ,args2) (-pad 'no-arg args1 args2)))
    (cons (cl-mapcar #'merge-type-single args1 args2)
          (merge-type-single out1 out2))))

(defun infer-signatures ()
  "Infer type signatures from runtime values."
  (maphash
   (lambda (fun all-values)
     (push (-reduce #'merge-types
                    (mapcar #'infer-type all-values))
           (gethash fun fun-to-values)))
   fun-to-values))

(defun pretty-print-values (values)
  "Pretty print collected VALUES for JSON export."
  (pcase-let ((`(,args . ,out) values))
    (list (cons "args" (mapcar (lambda (a) (format "%s" a)) args))
          (cons "return" (format "%s" out)))))

(defun export-to-json ()
  "Export type and value information to JSON."
  (with-temp-file "/tmp/types.json"
    ;; TODO: sort by lexical order on function name
    (let ((export (make-hash-table)))
      (maphash (lambda (fun values)
                 (puthash fun (mapcar #'pretty-print-values values) export))
               fun-to-values)
      (princ (format "%s" (json-encode-hash-table export)) (current-buffer)))))
