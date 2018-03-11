(tyck-type-of '(+ 1 1 1 1 1))
(tyck-type-of '(+ 1 "a"))

(tyck-type-of '(list 1 2 3))
(tyck-type-of '(cdr (list 1 "a" 3)))

(tyck-type-of '(cdr))
(tyck-type-of '(cdr (list) (list)))

(tyck-type-of '(< 1 2 3))
(tyck-type-of '(1+ 3))

(tyck-type-of '(let ((i "a")
                     (num 2)
                     (list (list 1 2 3)))
                 (while (and (< i num) list)
                   (setq list (cdr list))
                   (setq i (1+ i)))
                 list))
