# AGENTS.md

This project contains Emacs Lisp code intended to be MELPA-ready.
All `.el` files MUST comply with the rules below at all times.

Codex is expected to autonomously run formatting and lint checks,
apply automatic fixes, and iterate until clean.
Routine style or packaging decisions MUST NOT be escalated to the user.

---

## Emacs Lisp Coding Conventions

### File Structure (mandatory)

Every `.el` file MUST contain the following elements in this order:

1. File header line with `lexical-binding: t`
2. Package headers (`Version`, `URL`, `Package-Requires`)
3. `;;; Commentary:` section (non-empty)
4. `;;; Code:` marker
5. Lisp code
6. `(provide 'FEATURE)`
7. File end marker `;;; filename.el ends here`

If any element is missing, Codex MUST insert it automatically.

### Header requirements

The first line MUST follow this pattern:

```elisp
;;; filename.el --- One line summary -*- lexical-binding: t; -*-
````

Required headers (insert if missing):

```elisp
;; Version: 0.1.0
;; URL: https://github.com/<OWNER>/<REPO>  ;; TODO: replace with real URL
;; Package-Requires: ((emacs "29.1"))
```

Rules:

* Default minimum Emacs version is **29.1**.
* Do NOT lower the minimum Emacs version.
* If `org`, `org-id`, or `org-*` APIs are used, add `(org "9.6")`.
* Only add other dependencies if they are explicitly required.

### Commentary section

* `;;; Commentary:` MUST exist.
* It MUST contain at least one paragraph explaining the purpose of the file.
* It MUST be written in English.

### Code marker and footer

* `;;; Code:` MUST appear exactly once.
* The file MUST end with:

```elisp
(provide 'feature-name)
;;; filename.el ends here
```

The feature name SHOULD match the file base name.

---

## Docstring and Comment Rules (checkdoc-compatible)

* All public functions MUST have docstrings.
* Docstrings MUST be grammatically correct English sentences.
* The first line MUST be a complete sentence.
* Lisp symbols, mode names, and feature names in docstrings
  MUST be quoted using backquote + apostrophe.

Example:

* Correct: `` `org-mode' ``

* Incorrect: `org-mode`

* If checkdoc reports:

  > Argument ‘x’ should appear in the doc string
  > Codex MUST mention the argument once, using the exact capitalization requested.

* Comments in code MUST be written in English.

---

## Customization and Public Interface

* All user-facing packages MUST define a customization group via `defgroup`.
* Group docstrings follow the same docstring rules as functions.
* Public commands MUST be declared with `interactive`.

---

## Formatting and Style Rules

* No trailing whitespace is allowed.
* Indentation MUST follow Emacs Lisp indentation rules.
* Long lines:

  * Prefer reflowing docstrings and comments.
  * Do NOT arbitrarily wrap code unless readability improves.
* `fill-column` warnings MUST be resolved where reasonable.

---

## Automatic Lint / Format Workflow (Always On)

There is no Dev/Release gate.
All code is treated as **release-quality at all times**.

### Mandatory commands

* Format (auto-fix):
  `elisp-format.sh FILES...`

* Lint:
  `elisp-lint.sh FILES...`

### Required workflow

Whenever Codex creates or modifies any `.el` file:

1. Run `elisp-format.sh` on all changed `.el` files.
2. Run `elisp-lint.sh` on the same files.
3. If lint fails:

   * Apply automatic fixes according to the rules below.
   * Repeat steps (1)–(2) until clean.

Codex MUST NOT ask the user how to fix routine lint or style issues.

---

## Auto-Fix Rules (No Questions Allowed)

Codex MUST automatically fix the following without confirmation:

### Trailing whitespace

* Remove all trailing whitespace.

### Indentation

* Fix obvious indentation issues (use Emacs Lisp indentation).
* `elisp-lint` の `indent` 警告は誤検知が多いため、**無視してよい**。ブロッカーにしない。
* Prefer reindenting the smallest enclosing top-level form.

### checkdoc issues

* Insert missing `;;; Commentary:` or `;;; Code:`.
* Fix symbol quoting in docstrings.
* Add missing argument mentions concisely.

### package-lint issues

* Insert missing packaging headers (`Version`, `URL`, `Package-Requires`).
* Resolve dependency warnings using the default Emacs version (29.1).
* Satisfy `user-error`, `lexical-binding`, and similar requirements
  without downgrading Emacs version.

### fill-column

* Reflow comments and docstrings.
* Adjust code formatting only when it clearly improves readability.

---

## Failure Handling (No Interactive Questions)

If repeated auto-fix attempts cannot make `elisp-lint.sh` pass:

Codex MUST:

1. Output the remaining lint errors verbatim.
2. Show the exact diff of the proposed next fix.
3. Choose a **safe default** and apply it automatically.

Codex MUST stop and ask the user **only if** the change would:

* Break public API compatibility, or
* Change persistent data formats (e.g. database schema).

In all other cases, Codex MUST proceed autonomously.

---

## Generated Files

The following files are auto-generated and MUST NOT be committed:

* `*.elc`
* `*.eln`
* `*-autoloads.el`

They may be deleted at any time and SHOULD be ignored by version control.

---
## Project Notes

* `elisp-format.sh` and `elisp-lint.sh` live outside the repository but are on PATH, so Codex can run them directly from the sandbox.
* Struct name, accessors, and predicate are unified to the `org-tasktree-model-node-*` prefix; do not use the old names in future changes.
