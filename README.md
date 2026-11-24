# lisp-source-protocol

Lossless Common Lisp source trees on [Eclector](https://github.com/s-expressionists/Eclector), for static analysis and structural editing. Nick: `stack-lisp-source`. First pillar of [`lisp-mcp`](https://github.com/egao1980/lisp-mcp).

- **Never evaluates.** `#.` yields a `read-eval-placeholder` plus a `read-time-eval-refused` signal; `#S` is not constructed.
- **Never interns.** Symbols are `symbol-token`s (package string / `:current` / `:keyword` / `nil`, name). Unknown packages are fine.
- **Lossless.** Every node is a range of the original text; comments and `#|…|#` are `trivia-node`s. Every non-whitespace character is covered.
- **Reader conditionals are source.** Every `#+`/`#-` form is a `:feature-conditional` expression whatever the features: an excluded one (`feature-excluded-p`) is a form like an included one, and its branch is re-read as plain Lisp, so paths, outlines, definition locators, references and edits see every branch. A branch in another implementation's reader syntax stays one opaque form, its text kept. Features only decide `node-value` and `feature-excluded-p`.
- **Broken files stay navigable.** A form that runs into end of input is re-read up to the next column-0 `(` or `#+`/`#-`, so one typo damages one form. Each problem becomes an `imbalance` (kind, opener, suggested fix offset, confidence). Paren-count plus indentation inference, parinfer-style.

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
| `*source-features*` | feature list for `#+`/`#-` (default host `*features*`), compared by name: sets `feature-excluded-p`, never which forms exist |
| `feature-excluded-p` / `feature-conditional-forms` / `feature-prefix` | an excluded `#+`/`#-`; a conditional's branch nodes; the `#-sbcl` prefix text a branch form sits under |

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
| `replace-node` `insert-before` `insert-after` `delete-node` | input text must read clean (`edit-input-unreadable`); every `#+`/`#-` form in it is written and counted, whatever the features; its comments (`;`, `#|…|#`, trailing) are written where they were; inserted code is re-indented in place (see below) |
| `wrap-node` `splice-node` `raise-node` `transpose-nodes` | `wrap-node` re-indents the wrapped form |
| `move-node` | to before/after a target form, in the same file (one plan) or another (two plans: verify both, then commit) |
| `slurp-forward` / `-backward`, `barf-forward` / `-backward` | proper `(…)` lists only |
| `rename-symbol` | one plan per file; keeps package prefixes and case style; covers `defpackage` designators |
| `add-export` | appends in the existing designator style, or adds an `(:export …)` option |
| `repair-parens` | applies an `imbalance` fix; accepted only if problems decrease |

Layout of inserted code: after a top-level form it goes after a blank line; before one, it goes above the `;` comment lines that belong to that form, then a blank line. Forms the code puts on one line are split onto their own lines there (and in a list laid out one per line); otherwise the code keeps its own line breaks. Inside a list it goes on its own line, at the column of the siblings on their own lines, when the neighbouring sibling is on another line, or the anchor is last (first) and starts its line, or the code spans lines; else on the anchor's line. A comment that ends the anchor's line stays there; one after the last inserted form follows the closing parens; code that would end up after a comment moves to the next line. `move-node` and `slurp-forward` / `-backward` place a form that lands first or last the same way.

Insert, delete, transpose and move on the form of a `#+`/`#-` branch act on the whole conditional, prefix included. Refused with `edit-not-applicable`: other sibling edits under `#+`/`#-` (the feature expression, slurp / barf), quote or backquote prefixes, the consing dot, dotted lists. Nodes built by recovery signal `edit-on-recovered-node` (`continue` to force).

## Indentation

Code an edit inserts goes through `indent-code` (`*reindent*` nil only shifts continuation lines). It starts from the lists open where the code goes (`indent-state`), so a replaced `labels` binding or `handler-case` clause indents as one, and new elements of that list line up with the siblings already on their own lines. The rules follow the Emacs/SLIME style (SLIME's `slime-cl-indent`):

- an operator with *N* distinguished arguments indents them by 4 and its body by 2: `defun` 2, `handler-case` (clauses get their own body indent), `flet`/`labels`/`macrolet` (local definitions get `defun` bodies), `let`/`let*`/`handler-bind`/`restart-bind`/`symbol-macrolet` (bindings: a value on its own line goes 2 in), unknown `def*` 2, `with-*` / `do-*` 1;
- `cond` clauses go 2 in from the paren, or line up with the first clause when it is on the `cond` line (`(:rest 2)`);
- a call aligns its arguments with the first argument on the operator's line (or with a keyword/value run there), else one past the paren;
- data (quoted lists, vectors, keyword or non-symbol heads) aligns with the first element; `loop` keeps the input's clause layout;
- lines starting inside a string or `#|…|#` are untouched; moving a form never shifts them either.

`set-indentation` takes an integer, `:local-definitions`, `:bindings`, `(:clauses N)`, `(:rest N)` or a Swank-style spec (`(4 4 &body)`); `learn-indentation` reads the `&body` position of every `defmacro` in a set of sources; `reset-indentation` restores the table.

Against Emacs 29 with `slime-cl-indent` on 157 files (alexandria, bordeaux-threads, usocket, http-backend-async, this repo's `src/`), 83 % of top-level forms re-indent exactly as SLIME does, and 12 % of lines differ (0.2.5: 81 % and 18 %). Most of the rest: lambda lists with `&key` / `&optional` parameters on several lines (SLIME lines them up after the keyword), RT-style `deftest` (SLIME's generic `def*` rule; Rove's `deftest` is 1, as Swank reports it), `unwind-protect` (SLIME puts the protected form 5 in), arguments after a `,`, and `loop` clauses.

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
