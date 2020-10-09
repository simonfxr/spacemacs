;;; early-init.el --- Spacemacs Early Init File
;;
;; Copyright (c) 2020 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Before Emacs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. Emacs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init file.
;; This behavior would prevent Spacemacs's own package initialization from
;; running. However, Emacs 27 also loads the "early init" file (this file)
;; before it initializes the package manager, and Spacemacs can use this early
;; init file to prevent Emacs from initializing the package manager. (See
;; <http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b>.)
;;
;; Earlier Emacs versions do not load the early init file and do not initialize
;; the package manager before loading the init file, so this file is neither
;; needed nor loaded on those versions.
(setq package-enable-at-startup nil)
;;; spacemacs-early-init-tail.el --- Will be concatenated with the .spacemacs.d/init.el template

;; The following code is adapted from the file core/core-dotspacemacs.el
;; distributed by Spacemacs.
;; See https://github.com/syl20bnr/spacemacs/blob/master/core/core-dotspacemacs.el

(let* ((env (getenv "SPACEMACSDIR"))
       (env-dir (when env (expand-file-name (concat env "/"))))
       (env-init (and env-dir (expand-file-name "early-init.el" env-dir)))
       (no-env-dir-default (expand-file-name "~/.spacemacs.d/"))
       (default-init (expand-file-name "~/.spacemacs-early")))
  (defconst dotspacemacs-directory
    (cond
     ((and env (file-exists-p env-dir)) env-dir)
     ((file-exists-p no-env-dir-default) no-env-dir-default)
     (t nil))
    "Optional spacemacs directory, which defaults to
~/.spacemacs.d. This setting can be overridden using the
SPACEMACSDIR environment variable. If neither of these
directories exist, this variable will be nil.")

  (defvar dotspacemacs-early-filepath
    (let ((spacemacs-dir-init (when dotspacemacs-directory
                                (concat dotspacemacs-directory
                                        "early-init.el"))))
      (cond
       (env-init)
       ((file-exists-p default-init) default-init)
       ((and dotspacemacs-directory (file-exists-p spacemacs-dir-init))
        spacemacs-dir-init)
       (t default-init)))
    "Filepath to the early dotfile. If SPACEMACSDIR is given
then SPACEMACSDIR/early-init.el is used. Otherwise, if ~/.spacemacs-early
exists, then this is used. If ~/.spacemacs-early does not exist, then
check for early-init.el in dotspacemacs-directory and use this if it
exists. Otherwise, fallback to ~/.spacemacs-early"))

(when (and dotspacemacs-early-filepath
           (file-exists-p dotspacemacs-early-filepath))
  (load dotspacemacs-early-filepath))

(define-advice spacemacs//init-spacemacs-env (:override (&rest _args) sfx-ad)
  "Override `spacemacs//init-spacemacs-env' with a NOOP. We don't need this functionality."
  nil)

(define-advice spacemacs/load-spacemacs-env (:override (&rest _args) sfx-ad)
  "Override `spacemacs/load-spacemacs-env' with a NOOP. We don't need this functionality."
  nil)

;;; spacemacs-early-init-tail.el ends here
