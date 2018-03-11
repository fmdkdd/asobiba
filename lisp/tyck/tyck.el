;;; Tyck --- Type checker for Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'pcase)
  (require 'cl-lib))

(defun tyck-type-eq (a b)
  "Return whether type A is equal to type B."
  (or (eq a 'any)
      (eq b 'any)
      (eq a b)))

(define-error 'type-error
  "Type error")

(defun tyck-fail (msg &rest args)
  "Report a type error.

MSG and ARGS are passed to `format'."
  (signal 'type-error (apply #'format `(,msg ,@args))))

(defun tyck-type (expr env constraints)
  "Return the type of EXPR given ENV and CONSTRAINTS."
  (pcase expr

    ;; Special forms

    (`(progn . ,bodies)
     (let ((ty))
       (dolist (b bodies)
         (setq ty (tyck-type b env constraints)))
       ty))

    (`(let ,bindings . ,bodies)
     (progn
       (let ((newenv (copy-hash-table env)))

         ;; Type bindings
         (dolist (b bindings)
           (pcase b
             (`(,var ,def)
              (puthash var (tyck-type def env constraints) newenv))
             (_ (error "Malformed let binding %S" b))))

         ;; Return type is the type of the last body
         ;; TODO: we could desugar that into a progn
         (let ((ty))
           (dolist (b bodies)
             (setq ty (tyck-type b newenv constraints)))
           ty))))

    (`(setq ,sym ,val)
     ;; TODO: handle multiple affectations
     (let ((ty (tyck-type val env constraints)))
       (puthash sym ty env)
       ty))

    (`(while ,cond . ,bodies)
     (progn
       ;; Type of cond doesn't really matter, only for side effects (like changing
       ;; types in the environment), or type errors.
       (tyck-type cond env constraints)

       ;; Return type is the type of the last body
       ;; TODO: could desugar into progn
       (let ((ty))
         (dolist (b bodies)
           (setq ty (tyck-type b env constraints)))
         ty)))

    (`(if ,cond ,then . ,else)
     (tyck-type cond env constraints)
     (let ((then-ty (tyck-type then env constraints))
           (else-ty 'unit))
       (dolist (b else)
         (setq else-ty (tyck-type b env constraints)))

       ;; Type of IF is the union of THEN and ELSE types
       (if (eq then-ty else-ty)
           then-ty
         (list then-ty else-ty))))

    ((or
      `(or . ,conds)
      `(and . ,conds))
     (let ((types))
       (dolist (e conds)
         (cl-pushnew (tyck-type e env constraints) types))
       (if (= 1 (length types))
           (car types)
         types)))

    ;; Functions

    (`(,fun . ,args)
     (pcase (tyck-type fun env constraints)
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
               (let ((actual (tyck-type (pop args) env constraints)))
                 (unless (tyck-type-eq expected actual)
                   (tyck-fail "Expected type %S, got type %S in call for function %S"
                              expected actual fun)))))

            (`,expected
             (let ((a (pop args)))
               (if (null a)
                   (tyck-fail "Not enough arguments to function %S" fun)
                 (let ((actual (tyck-type a env constraints)))
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
        (constraints))
    (list (tyck-type expr env constraints)
          env
          constraints)))

(provide 'tyck)

;; TODO: the byte-compiled version makes the tests fail.
;; Emacs bug?

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tyck.el ends here
