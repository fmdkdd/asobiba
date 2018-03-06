extern crate nil_checker;

use nil_checker::parser::TokenStream;

fn main() {
  let mut t = TokenStream::new(r#"
(defun flycheck-command-checker-print-doc (checker)
  "Print additional documentation for a command CHECKER."
  (let ((executable (flycheck-checker-default-executable checker))
        (config-file-var (flycheck-checker-get checker 'config-file-var))
        (option-vars (seq-sort #'string<
                               (flycheck-checker-get checker 'option-vars))))
    (princ "\n")

    (let ((doc-start (with-current-buffer standard-output (point-max))))
      ;; Track the start of our documentation so that we can re-indent it
      ;; properly
      (princ "  This syntax checker executes \"")
      (princ executable)
      (princ "\"")
      (when config-file-var
        (princ ", using a configuration file from `")
        (princ (symbol-name config-file-var))
        (princ "'"))
      (princ ". The executable can be overridden with `")
      (princ (symbol-name (flycheck-checker-executable-variable checker)))
      (princ "'.")

      (with-current-buffer standard-output
        (save-excursion
          (fill-region-as-paragraph doc-start (point-max)))))
    (princ "\n")
    (when option-vars
      (princ "\n  This syntax checker can be configured with these options:\n\n")
      (dolist (var option-vars)
        (princ (format "     * `%s'\n" var))))))
"#);

  while !t.is_eof() {
    println!("{:?}", t.next());
  }
}
