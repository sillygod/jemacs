# jemacs

Jing's Emacs configuration, written as literate org files.  The repo is
public (github.com/sillygod/jemacs): nothing personal or secret goes in a
tracked file.

## The config is tangled from org

- Edit the `*_lp.org` files -- `early_init_lp.org`, `init_lp.org`,
  `core/*_lp.org`, `modules/*_lp.org` -- never the `.el` they tangle to
  (`early-init.el`, `init.el`, `core/*.el`, `modules/*.el`, `settings.el`).
  Those are generated and git-ignored: an edit there is lost at the next
  tangle and never reaches git.
- Each org file names its target in its `#+PROPERTY: header-args:emacs-lisp
  :tangle ...` line; a block marked `:tangle no` is not tangled.
- Explain a setting in comments inside its block, as the org files do.
- Tangle after editing, then diff the tangled `.el` to see only what you
  meant changed:
  - all of them: `bash lp.sh`
  - one file, from the repo root:

    ```sh
    emacs --batch --init-directory="$PWD/emacs-home/" \
      --eval "(require 'org)" --eval "(require 'ob-tangle)" \
      --eval '(find-file "modules/jemacs_ai_lp.org")' --eval '(org-babel-tangle)'
    ```

## Personal settings

- Machine-specific values, keys and tokens go in `personal_lp.org` (tangles
  to `settings.el`, loaded last) or `config.el` (loaded first, plain `setq`
  only) -- both git-ignored.  `example-personal_lp.org` says which one a
  value belongs in.
- `emacs-home/` is straight.el's tree, not config: never edit it.

## Local packages

- `local/<pkg>/` are packages of their own, loaded by straight from a
  `:local-repo`: plain `.el`, edited directly, each with its own tests.
- straight does not rebuild them by itself: after a change, the next Emacs
  restart needs `M-x straight-rebuild-package RET <pkg>` first.
- A ghostherd or xwapp change reaches the running Emacs with
  `local/ghostherd/bin/reload FILE...` -- every changed file in one call.
