# q-ts-capf

Completion at point functions leveraging
[treesitter-q](https://github.com/Gchouchou/tree-sitter-q) for
[q-ts-mode](https://github.com/Gchouchou/q-ts-mode)
extending [q-capf](https://github.com/Gchouchou/q-capf).

# Completion at Point Functions

This package provides various extra completion at point functions that leverage
tree-sitter to get information. These functions are all combined together (candidates are combined)
with the function `q-ts-capf-super-capf`

| capf-name                  | Description                                      |
|----------------------------|--------------------------------------------------|
| `q-ts-capf-local-variable` | suggest local variables from function definition |
| `q-ts-capf-table-col-capf` | suggest table column names in q-SQL queries      |

# Eldoc Functions

The eldoc function from `q-capf` has been slightly improved with better
hilighting on parameter pass.

# Example Config

The following config sets completion at point function and the eldoc function
for `q-ts-mode` buffers.

``` emacs-lisp
(require 'corfu)
(global-corfu-mode)

(require 'q-ts-mode)
(require 'q-ts-capf)

(add-hook 'q-ts-mode-hook (lambda () (setq-local completion-at-point-functions (list #'q-ts-capf-super-capf))
                         (add-hook 'eldoc-documentation-functions #'q-capf-eldoc -100 t)))
```
