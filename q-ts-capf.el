;;; q-ts-capf.el --- Completion at Point Powered by Treesitter

;; Author: Justin Yu
;; Homepage: https://github.com/Gchouchou/q-ts-capf
;; Created 23 Apr 2025
;; Version: 0.1

;;; Commentary:

;;; Requirements:
;;; Package-Requires: ((emacs "29.1") (q-ts-mode) (q-capf))

;;; Code:
(require 'q-capf)
(require 'q-ts-mode)

(defvar q-ts-capf--columns
  ()
  "Cache of columns.")

(defun q-ts-capf--bounds (&optional default)
  "Return bounds of token in q grammar.
Calls DEFAULT if there are no matches."
  (let* ((node (treesit-node-at (point) 'q))
         (parent (when node (treesit-node-parent node))))
    (cond
     ((and node (string-match-p
                 (eval-when-compile
                   (format
                    "^%s$"
                    (regexp-opt
                     (list "builtin_infix_func" "assignment_func"
                           "number" "temporal" "symbol" "invalid_atom"
                           "command" "byte_list"))))
                 (treesit-node-type node))
           (<= (treesit-node-start node) (point) (treesit-node-end node)))
      (cons (treesit-node-start node) (treesit-node-end node)))
     ((and parent (string-match-p
                   (eval-when-compile
                     (format
                      "^%s$"
                      (regexp-opt
                       (list "variable" "char" "string"))))
                   (treesit-node-type parent))
           (<= (treesit-node-start parent) (point) (treesit-node-end parent)))
      (cons (treesit-node-start parent) (treesit-node-end parent)))
     (t (if default (funcall default) (cons (point) (point)))))))

;; override q-capf--bounds with treesitter version
(advice-add 'q-capf--bounds :around 'q-ts-capf--bounds)

;;;###autoload
(defun q-ts-capf-table-col-capf ()
  "Completion at point for table column names."
  (when (and (hash-table-p q-capf-session-vars)
             ;; do not trigger inside comments and strings
             (not (nth 3 (syntax-ppss)))
             (not (nth 4 (syntax-ppss))))
    ;; we need to make sure we have a sql expression
    (when-let* ((node (treesit-node-at
                       (save-excursion
                         ;; while it is space or tab
                         (while (or (eq (char-before) 32) (eq (char-before) 10))
                           (backward-char))
                         (backward-char)
                         (point))
                       'q))
                (sql_exp (treesit-parent-until
                          node
                          (lambda (node)
                            ;; this could be an error when not valid
                            (string-match-p "\\(sql_expression\\|comment\\|string\\)"
                                     (treesit-node-type node)))
                          t))
                (table-node (when (string= "sql_expression" (treesit-node-type sql_exp))
                                (treesit-node-child-by-field-name sql_exp "table")))
                ;; only continue if table is a variable
                (table (and (string= "variable" (treesit-node-type table-node))
                            (treesit-node-text table-node)))
                ;; double check we have documentation and more than 1 column for table
                (element (treesit-node-text (treesit-node-child-by-field-name table-node "element")))
                (namespace (substring table 0 (- (length table) (length element))))
                (doc (q-capf-get-doc element namespace))
                (columns (gethash "cols" doc))
                ;; get the nodes/positions of all keywords
                (keyword-children (treesit-filter-child
                                   sql_exp
                                   (lambda (node)
                                     (string-match-p
                                      "^\\(select\\|exec\\|delete\\|update\\|by\\|from\\|where\\)$"
                                      ;; type works on exact matches
                                      (treesit-node-text node)))))
                (command (when (string-match-p "^\\(select\\|exec\\|delete\\|update\\)$"
                                               (treesit-node-text (car keyword-children)))
                           (pop keyword-children)))
                (from (progn (while (and keyword-children
                                         (not (string= "from" (treesit-node-text (car keyword-children)))))
                               (pop keyword-children))
                             (when keyword-children (pop keyword-children)))))
      ;; only succeed when point is between end of command and start of from
      (when (or (< (treesit-node-end command) (point) (treesit-node-start from))
                ;; or when it is after the "where" command
                (when (and keyword-children (string= "where" (treesit-node-text (car keyword-children))))
                  (< (treesit-node-end (car keyword-children)) (point))))
        (setq q-ts-capf--columns (append columns nil))
        (let ((bounds (q-capf--bounds)))
          (list
           (car bounds)
           (cdr bounds)
           (append columns nil)
           :exclusive 'no
           :annotation-function
           (lambda (col) " table column")))))))

;;;###autoload
(defun q-ts-capf-super-capf ()
  "Wrap `q-ts-capf-table-col-capf' and `q-capf-completion-at-point'."
  (let* ((col-capf (q-ts-capf-table-col-capf))
         (capf (q-capf-completion-at-point)))
    (cond
     ;; both present and same begin and end
     ((and col-capf capf (eq (car col-capf) (car capf))
           (eq (cadr col-capf) (cadr capf)))
      (let* ((begin (car capf))
             (end (cadr capf))
             (col-candidates (caddr col-capf))
             (capf-candidates (caddr capf))
             (col-plist (cdddr col-capf))
             (capf-plist (cdddr capf)))
        (list
         begin
         end
         (append col-candidates capf-candidates)
         :exclusive 'no
         :annotation-function
         (lambda (candidate)
            (if (member candidate q-ts-capf--columns)
                " table column"
              (q-capf--capf-annotation candidate)))
         :company-doc-buffer
         (lambda (candidate)
            (unless (member candidate q-ts-capf--columns)
                (q-capf--capf-doc-buffer candidate))))))
     ;; uneven bounds
     ((and col-capf capf)
      col-capf)
     ;; one present or all missing
     (t (or col-capf capf)))))

(defun q-ts-capf-eldoc-get-bounds (&optional default)
  "Around Function for DEFAULT `q-capf-eldoc-get-bounds'.
Used by `q-capf-eldoc' to get the bounds of q variable/function.
Leverage treesitter to accurately get bounds.
Returns ((start . end) . index), where index is used for function parameters."
  (or
   (save-excursion
     (let* ((bounds (q-ts-capf--bounds))
            (pos (point)))
       (cond
        ((or (bobp) (not (eq (car bounds) (cdr bounds)))) (cons bounds -1))
        ((progn (skip-chars-backward " \t")
                (eq ?w (char-syntax (char-before))))
         ;; possibly the first argument of the function
         (goto-char (- (point) 1))
         (cons (q-ts-capf--bounds) 0))
        ;; try to find parameter pass
        (t (condition-case nil
               (progn
                 (backward-up-list)
                 (when (eq ?\[ (char-after))
                   (if-let*
                       ((parameter_pass (treesit-node-parent
                                         (treesit-node-at (+ 1 (point)))))
                        (n (length
                            (treesit-filter-child
                             parameter_pass
                             (lambda (child)
                               (and (string= "semicolon" (treesit-node-type child))
                                    (<= (treesit-node-end child) pos))))))
                        (bounds (progn
                                  ;; goto function definition, end-1
                                  (goto-char
                                   (- (treesit-node-end
                                       (treesit-node-child-by-field-name
                                        (treesit-node-parent parameter_pass)
                                        "function"))
                                      1))
                                  (q-ts-capf--bounds))))
                       (cons bounds n)
                     (cons bounds -1))))
             (scan-error nil))))))
   (when default (funcall default))))

;;; override q-capf--bounds with treesitter version
(advice-add 'q-capf-eldoc-get-bounds :around 'q-ts-capf-eldoc-get-bounds)

(provide 'q-ts-capf)
;;; q-ts-capf.el ends here
