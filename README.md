# lisp-source-protocol

Lossless Common Lisp source trees on [Eclector](https://github.com/s-expressionists/Eclector), for static analysis and structural editing. Nick: `stack-lisp-source`. First pillar of [`lisp-mcp`](https://github.com/egao1980/lisp-mcp).

- **Never evaluates.** `#.` yields a `read-eval-placeholder` plus a `read-time-eval-refused` signal; `#S` is not constructed.
- **Never interns.** Symbols are `symbol-token`s (package string / `:current` / `:keyword` / `nil`, name). Unknown packages are fine.
- **Lossless.** Every node is a range of the original text; comments, `#|…|#` and skipped `#+`/`#-` forms are `trivia-node`s. Every non-whitespace character is covered.
- **Broken files stay navigable.** A form that runs into end of input is re-read up to the next column-0 `(`, so one typo damages one form. Each problem becomes an `imbalance` (kind, opener, suggested fix offset, confidence). Paren-count plus indentation inference, parinfer-style.

```lisp
(asdf:load-system "lisp-source-protocol")

(let ((ps (stack-lisp-source:read-source
           "(defun a (x)
  (let ((y 1)))
    (+ x y)))
(defun b () 2)")))
  (values (mapcar #'stack-lisp-source:node-text (stack-lisp-source:parsed-source-forms ps))
          (stack-lisp-source:parsed-source-imbalances ps)))
;; => ("(defun a (x) …" "(defun b () 2)")
;;    (#<IMBALANCE :EXTRA-CLOSE x1 form@0 fix@27 :MEDIUM>)   ; the early ) of the LET
```

| API | |
|-----|--|
| `read-source` / `read-source-file` | → `parsed-source` (`-forms`, `-nodes`, `-diagnostics`, `-imbalances`, `-hash`, `-clean-p`) |
| `node-kind` / `-start` / `-end` / `-children` / `-forms` / `-parent` / `-path` / `-text` / `-hash` / `-recovered-p` | node accessors |
| `locate` | `(:path 0 2)`, `(:offset n)`, `(:line l :column c)`, `(:definition "defmethod" "print-object" "(my-class t)")`, `(:hash h)`; `ambiguous-locator` offers `use-value` |
| `outline` | top-level definitions with docstring, lambda list, package |
| `*source-features*` | feature list for `#+`/`#-` (default host `*features*`), compared by name |

## Test

```bash
ros -e '(asdf:test-system "lisp-source-protocol")' -q
# whole-tree corpus + mutation study (hidden dirs skipped):
LISP_SOURCE_CORPUS=~/Projects/cl-workspace ros run -- --dynamic-space-size 4096 \
  --non-interactive --eval '(asdf:test-system "lisp-source-protocol")'
```

Workspace run (2026-11-06): 2,894 files fully covered; 24,922 single-paren mutations — 99.6 % keep every other top-level form byte-identical, 100 % detected, 91.6 % fix within 2 lines.

## License

MIT
