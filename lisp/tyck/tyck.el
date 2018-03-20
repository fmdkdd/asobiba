;;; Tyck --- Type checker for Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'pcase)
  (require 'cl-lib))

(define-error 'type-error
  "Type error")

(defvar-local tyck--constraints nil)

(defun tyck-type-eq (a b)
  "Return whether type A is equal to type B."
  (cond
   ((eq a 'any) t)
   ((eq b 'any) t)
   ((and (listp a) (eq (car a) 'type-var))
    (cl-pushnew `(= ,a ,b) tyck--constraints :test #'equal)
    t)
   ((and (listp b) (eq (car b) 'type-var))
    (cl-pushnew `(= ,b ,a) tyck--constraints :test #'equal)
    t)
   ((eq a b) t)
   (t nil)))

(defun tyck-subst1 (a b sym)
  (if (equal sym a)
      b
    sym))

(defun tyck-subst (a b constraints)
  (mapcar
   (lambda (cstr)
     (pcase cstr
       (`(= ,left ,right)
        `(= ,(tyck-subst1 a b left)
            ,(tyck-subst1 a b right)))

       (_ (error "Malformed constraint %S" cstr))))
   constraints))

(defun tyck-apply-substs (type substs)
  (let ((result type))
    (dolist (sub substs)
      (pcase sub
        (`(,left -> ,right)
         (when (eq left type)
            (setq result right)))

        (_ (error "Malformed substitution %S" sub))))
    result))

(defun tyck-unify (constraints)
  (if (null constraints)
      '()
    (let ((head (car constraints))
          (tail (cdr constraints)))
      (pcase head
        (`(= ,left ,right)

         (cond
          ((eq left right)
           (tyck-unify tail))
          ((and (listp left) (eq (car left) 'type-var))
           (cons `(,left -> ,right) (tyck-unify (tyck-subst left right tail))))
          ((and (listp right) (eq (car right) 'type-var))
           (cons `(,right -> ,left) (tyck-unify (tyck-subst right left tail))))
          (t (error "Failed to unify %S" head))))

        (_ (error "Malformed constraints %S" head))))))

(defun tyck-fail (msg &rest args)
  "Report a type error.

MSG and ARGS are passed to `format'."
  (signal 'type-error (apply #'format `(,msg ,@args))))

(defun tyck-type (expr env)
  "Return the type of EXPR given ENV and CONSTRAINTS."
  (pcase expr

    ;; Special forms

    (`(progn . ,bodies)
     (let ((ty))
       (dolist (b bodies)
         (setq ty (tyck-type b env)))
       ty))

    (`(let ,bindings . ,bodies)
     (let ((newenv (copy-hash-table env)))

       ;; Type bindings
       (dolist (b bindings)
         (pcase b
           (`(,var ,def)
            (puthash var (tyck-type def env) newenv))
           (_ (error "Malformed let binding %S" b))))

       ;; Return type is the type of the last body
       ;; TODO: we could desugar that into a progn
       (let ((ty))
         (dolist (b bodies)
           (setq ty (tyck-type b newenv)))
         ty)))

    (`(setq ,sym ,val)
     ;; TODO: handle multiple affectations
     (let ((ty (tyck-type val env)))
       (puthash sym ty env)
       ty))

    (`(while ,cond . ,bodies)
     (progn
       ;; Type of cond doesn't really matter, only for side effects (like changing
       ;; types in the environment), or type errors.
       (tyck-type cond env)

       ;; Return type is the type of the last body
       ;; TODO: could desugar into progn
       (let ((ty))
         (dolist (b bodies)
           (setq ty (tyck-type b env)))
         ty)))

    (`(if ,cond ,then . ,else)
     (tyck-type cond env)
     (let ((then-ty (tyck-type then env))
           (else-ty 'unit))
       (dolist (b else)
         (setq else-ty (tyck-type b env)))

       ;; Type of IF is the union of THEN and ELSE types
       (if (eq then-ty else-ty)
           then-ty
         (list then-ty else-ty))))

    (`(defun ,name ,arglist . ,bodies)

     ;; Create a type variable for each argument, and try
     ;; to type the body with that.
     (let ((newenv (copy-hash-table env))
           (arg-types)
           (return-type))
       (dolist (arg arglist)
         (puthash arg (list 'type-var (make-symbol (symbol-name arg))) newenv))

       (dolist (b bodies)
         (setq return-type (tyck-type b newenv)))

       ;; Gather arg types
       (dolist (arg arglist)
         (push (tyck-apply-substs (gethash arg newenv)
                                  (tyck-unify tyck--constraints))
               arg-types))

       ;; Return type is a function from all args to the return type
       (let ((fun-type `(fun ,(nreverse arg-types) ,return-type)))
         ;; Populate environment
         (puthash name fun-type env)
         fun-type)))

    ((or
      `(or . ,conds)
      `(and . ,conds))
     (let ((types))
       (dolist (e conds)
         (cl-pushnew (tyck-type e env) types))
       (if (= 1 (length types))
           (car types)
         types)))

    ;; Functions

    (`(,fun . ,args)
     (pcase (tyck-type fun env)
       (`(fun ,arg-types ,ret)

        ;; TODO: Check arity up front

        ;; At least the number of required parameters, plus any thing if there
        ;; are rest parameters.

        ;; (unless (eq (length arg-types) (1+ (length args)))
        ;;   (error "Function %S needs %S arguments, but called with %S"
        ;;          fun (1- (length arg-types)) (length args))))

        ;; Check argument types
        (dolist (ty arg-types)
          (pcase ty
            (`(&rest ,expected)
             (while (not (null args))
               (let ((actual (tyck-type (pop args) env)))
                 (unless (tyck-type-eq expected actual)
                   (tyck-fail "Expected type %S, got type %S in call for function %S"
                              expected actual fun)))))

            (`,expected
             (let ((a (pop args)))
               (if (null a)
                   (tyck-fail "Not enough arguments to function %S" fun)
                 (let* ((actual (tyck-type a env)))
                   (unless (tyck-type-eq expected actual)
                     (tyck-fail "Expected type %S, got type %S in call for function %S"
                                expected actual fun))))))))

        (unless (null args)
          (tyck-fail "Too many arguments to function %S" fun))

        ;; Type of call is the return type of the function
        ret)

       (_ (error "%S is not a function" fun))))

    ;; Base types

    ((pred numberp) 'number)
    ((pred stringp) 'string)
    ((pred booleanp) 'boolean)
    ((pred symbolp)
     (or (gethash expr env)
         (puthash expr (make-symbol (format "type-%S" expr)) env)))

    (_ (error "Cannot emit constraint for pattern %S" expr))))

(defvar tyck-default-env
  #s(hash-table data
                (+ (fun ((&rest number)) number)
                   < (fun (number (&rest number)) bool)
                   1+ (fun (number) number)
                   cdr (fun (list) list)
                   list (fun ((&rest any)) list))))

(defun tyck-type-of (expr)
  "Return the type of EXPR, a Lisp expression."
  (let ((env (copy-hash-table tyck-default-env))
        (tyck--constraints nil))
    (list (tyck-type expr env)
          ;env
          (tyck-unify tyck--constraints))))

(provide 'tyck)

;; TODO: the byte-compiled version makes the tests fail.
;; Emacs bug?

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tyck.el ends here
