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

(defvar q-ts-capf--params
  ()
  "Cache of param names.")

(defun q-ts-capf--bounds (&optional default)
  "Return bounds of token in q grammar.
Calls DEFAULT if there are no matches."
  (or (save-excursion
        (skip-syntax-backward "w")
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
            (cons (treesit-node-start parent) (treesit-node-end parent))))))
      (when default (funcall default))
      (cons (point) (point))))

;; override q-capf--bounds with treesitter version
(advice-add 'q-capf--bounds :around 'q-ts-capf--bounds)

;;;###autoload
(defun q-ts-capf-table-col-capf ()
  "Completion at point for table column names."
  (interactive)
  (setq q-ts-capf--columns nil)
  (when (and (hash-table-p q-capf-session-vars)
             ;; do not trigger inside comments and strings
             (not (nth 3 (syntax-ppss)))
             (not (nth 4 (syntax-ppss))))
    (when-let* ((node (save-excursion
                        (while (or (eq (char-before) ? ) (eq (char-before) ?\t))
                          (backward-char))
                        (unless (or (bobp) (eq (char-before) ?\n)) (backward-char))
                        (let* ((n (treesit-node-at (point) 'q)))
                          ;; match where and , ERROR nodes due to incomplete SQL statements
                          (if (and (string-match-p "^\\(where\\|,\\)$" (treesit-node-text n))
                                   (string= "ERROR" (treesit-node-type (treesit-node-parent n))))
                              (progn
                                (goto-char (treesit-node-start n))
                                (while (or (eq (char-before) ? ) (eq (char-before) ?\t))
                                  (backward-char))
                                (unless (or (bobp) (eq (char-before) ?\n)) (backward-char))
                                (treesit-node-at (point) 'q))
                            n))))
                (sql_exp (treesit-parent-until
                          node
                          (lambda (node)
                            ;; func_definition creates a new closure
                            (string-match-p "\\(sql_expression\\|func_definition\\)"
                                     (treesit-node-type node)))
                          t))
                (table-node (when (string= "sql_expression" (treesit-node-type sql_exp))
                                (treesit-node-child-by-field-name sql_exp "table")))
                (table (when (string= "variable" (treesit-node-type table-node))
                         (treesit-node-text table-node)))
                ;; double check we have documentation and more than 1 column for table
                (element (treesit-node-text (treesit-node-child-by-field-name table-node "element")))
                (namespace (substring table 0 (- (length table) (length element))))
                (doc (q-capf-get-doc element namespace))
                (columns (gethash "cols" doc))
                (sql-children (treesit-node-children sql_exp))
                (command (when (string-match-p "^\\(select\\|exec\\|delete\\|update\\)$"
                                               (treesit-node-text (car sql-children)))
                           (pop sql-children)))
                (from (let* ((front (pop sql-children)))
                        (while (and front (not (string= "from" (treesit-node-text front))))
                          (setq front (pop sql-children)))
                        front)))
      (when (or (< (treesit-node-end command) (point) (treesit-node-start from))
                (< (treesit-node-end table-node) (point)))
        (setq q-ts-capf--columns (append columns nil))
        (let ((bounds (q-ts-capf--bounds)))
          (list
           (car bounds)
           (cdr bounds)
           (append columns nil)
           :exclusive 'no
           :annotation-function
           (lambda (col) " table column")))))))

;;;###autoload
(defun q-ts-capf-local-variable ()
  "Completion at point for function local variables."
  (interactive)
  ;; do not trigger inside comments and strings
  (setq q-ts-capf--params nil)
  (when (and (not (nth 3 (syntax-ppss)))
             (not (nth 4 (syntax-ppss))))
    (when-let* ((bounds (q-ts-capf--bounds))
                ;; find the first ancestor func_definition
                (func_def (treesit-parent-until
                           (treesit-node-at (car bounds))
                           (lambda (node)
                             (string= "func_definition" (treesit-node-type node)))
                           t))
                (candidates
                 (if-let* ((param_node (treesit-node-child-by-field-name func_def "parameters")))
                     (let* ((params (treesit-filter-child
                                     param_node
                                     (lambda (node)
                                       (string= "variable" (treesit-node-type node)))
                                     'named)))
                       (if (> (length params) 0)
                           (mapcar (lambda (param)
                                     (let* ((text (treesit-node-text param)))
                                       (set-text-properties 0 (length text) nil text)
                                       text))
                                   params)
                         nil))
                   ;; default parameter names
                   '("x" "y" "z"))))
      (setq q-ts-capf--params candidates)
      (list
       (car bounds)
       (cdr bounds)
       candidates
       :exclusive 'no
       :annotation-function
       (lambda (cand) " local function parameter")))))

;;;###autoload
(defun q-ts-capf-super-capf ()
  "Wrap all q-capfs.
Combines `q-ts-capf-table-col-capf', `q-capf-completion-at-point'
and `q-ts-capf-local-variable'."
  (interactive)
  (when (and (not (nth 3 (syntax-ppss)))
             (not (nth 4 (syntax-ppss))))
    (let* ((bounds (q-ts-capf--bounds))
           (begin (car bounds))
           (end (cdr bounds)))
      ;; if there is a period before only capf is valid
      (if (when (char-after begin)
            (char-equal ?. (char-after begin)))
          (q-capf-completion-at-point)
        (let* ((col-capf (q-ts-capf-table-col-capf))
               (param-capf (q-ts-capf-local-variable))
               (capf (q-capf-completion-at-point))
               (candidates (when capf (caddr capf))))
          (setq q-ts-capf--params (seq-difference q-ts-capf--params candidates))
          (list
           begin
           end
           (append q-ts-capf--columns q-ts-capf--params candidates)
           :exclusive 'no
           :annotation-function
           (lambda (candidate)
             (cond
              ((member candidate q-ts-capf--columns) " table column")
              ((member candidate q-ts-capf--params) " local function parameter")
              (t (q-capf--capf-annotation candidate))))
           :company-doc-buffer
           (lambda (candidate)
             (unless (or (member candidate q-ts-capf--columns)
                         (member candidate q-ts-capf--params))
               (q-capf--capf-doc-buffer candidate)))))))))

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
         (goto-char (1- (point)))
         (cons (q-ts-capf--bounds) 0))
        ;; try to find parameter pass
        (t (condition-case nil
               (progn
                 (backward-up-list)
                 (when (eq ?\[ (char-after))
                   (if-let*
                       ((parameter_pass (treesit-node-parent
                                         (treesit-node-at (point))))
                        (func_node (treesit-node-child-by-field-name
                                    (treesit-node-parent parameter_pass)
                                    "function"))
                        (n (length
                            (treesit-filter-child
                             parameter_pass
                             (lambda (child)
                               (and (string= "semicolon" (treesit-node-type child))
                                    (<= (treesit-node-end child) pos)))))))
                       (cons (progn ;; goto function definition, end-1
                               (goto-char (1- (treesit-node-end func_node)))
                               (q-ts-capf--bounds))
                             n)
                     (cons bounds -1))))
             (scan-error nil))))))
   (when default (funcall default))))

;;; override q-capf-eldoc-get-bounds with treesitter version
(advice-add 'q-capf-eldoc-get-bounds :around 'q-ts-capf-eldoc-get-bounds)

(provide 'q-ts-capf)
;;; q-ts-capf.el ends here
