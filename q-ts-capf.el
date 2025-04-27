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
        (let ((bounds (q-capf--bounds)))
          (list
           (car bounds)
           (cdr bounds)
           (append columns nil)
           :exclusive 'no
           :annotation-function
           (lambda (col) " table column")))))))

(provide 'q-ts-capf)
;;; q-ts-capf.el ends here
