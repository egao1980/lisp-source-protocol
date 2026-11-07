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
| `build-package-model` / `resolve-symbol-token` / `package-at` | static package model from `defpackage` / `define-package` (`:use`, `:import-from`, `:shadow`, `:local-nicknames`, nicknames); host packages via `find-package` (lookup only) |
| `find-references` / `find-definitions` | across sources, by resolved package + name; includes `defpackage` designators |
| `lint` / `*lint-rules*` / `run-lint-rule` | `:unused-variable` `:missing-in-package` `:undefined-export` `:missing-docstring` `:asd-missing-component`; add a rule with an `eql` method |
| `*source-features*` | feature list for `#+`/`#-` (default host `*features*`), compared by name |

## Structural editing

Operations never write text: each returns an `edit-plan` (non-overlapping replacements + expected top-level form delta). `verify-plan` re-reads the result and signals `edit-would-unbalance` if it adds a reader error or imbalance, or changes the form count unexpectedly. `commit-plan` verifies, checks `:expected-hash` and the file on disk (`stale-source`, `continue` restart), writes atomically and returns an `edit-result` with a unified diff.

```lisp
(let* ((ps (stack-lisp-source:read-source-file "src/foo.lisp"))
       (node (stack-lisp-source:locate ps '(:definition "defun" "frob"))))
  (stack-lisp-source:commit-plan
   (stack-lisp-source:replace-node ps node "(defun frob (x) (1+ x))")
   :expected-hash (stack-lisp-source:parsed-source-hash ps)))
```

| Ops | |
|-----|--|
| `replace-node` `insert-before` `insert-after` `delete-node` | input text must read clean (`edit-input-unreadable`); inserted code is re-indented at the node's column (see below) |
| `wrap-node` `splice-node` `raise-node` `transpose-nodes` | `wrap-node` re-indents the wrapped form |
| `move-node` | to before/after a target form, in the same file (one plan) or another (two plans: verify both, then commit) |
| `slurp-forward` / `-backward`, `barf-forward` / `-backward` | proper `(…)` lists only |
| `rename-symbol` | one plan per file; keeps package prefixes and case style; covers `defpackage` designators |
| `add-export` | appends in the existing designator style, or adds an `(:export …)` option |
| `repair-parens` | applies an `imbalance` fix; accepted only if problems decrease |

Refused with `edit-not-applicable`: sibling edits under `#+`/`#-`, quote or backquote prefixes, the consing dot, dotted lists. Nodes built by recovery signal `edit-on-recovered-node` (`continue` to force).

## Indentation

Code an edit inserts goes through `indent-code` (`*reindent*` nil only shifts continuation lines). The rules follow the Emacs/SLIME style:

- an operator with *N* distinguished arguments indents them by 4 and its body by 2: `defun` 2, `let` 1, `handler-case` (clauses get their own body indent), `flet`/`labels` (local definitions), unknown `def*` 2, `with-*` / `do-*` 1;
- a call aligns its arguments with the first argument on the operator's line (or with a keyword/value run there), else one past the paren;
- data (quoted lists, vectors, keyword or non-symbol heads) aligns with the first element; `loop` keeps the input's clause layout;
- lines starting inside a string or `#|…|#` are untouched.

`set-indentation` takes an integer or a Swank-style spec (`(4 4 &body)`); `learn-indentation` reads the `&body` position of every `defmacro` in a set of sources; `reset-indentation` restores the table. On this repo's and the cl-stack protocol sources, 85 % of top-level forms re-indent to exactly their current text; the rest are mostly calls to project macros.

## Test

```bash
ros -e '(asdf:test-system "lisp-source-protocol")' -q
# whole-tree corpus + mutation study (hidden dirs skipped):
LISP_SOURCE_CORPUS=~/Projects/cl-workspace ros run -- --dynamic-space-size 4096 \
  --non-interactive --eval '(asdf:test-system "lisp-source-protocol")'
```

Workspace run (2026-11-06): 2,906 files fully covered; 25,042 single-paren mutations — 99.7 % keep every other top-level form byte-identical, 100 % detected, 91.5 % fix within 2 lines; 34,256 random structural operations verified, none left a file unclean. 0.2.0 (2026-11-07), with move, re-indenting wrap and multi-line replace added to the random set: 39,875 verified, 15,708 refused, none unclean.

## License

MIT
