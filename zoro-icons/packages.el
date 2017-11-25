;;; packages.el --- zoro-icons layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: zoro <zoro@zoro-HP>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `zoro-icons-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `zoro-icons/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `zoro-icons/pre-init-PACKAGE' and/or
;;   `zoro-icons/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst zoro-icons-packages
  '(all-the-icons
    neotree
    all-the-icons-dired)
  "The list of Lisp packages required by the zoro-icons layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")
(defun zoro-icons/init-all-the-icons ()
  "Initialize all-the-incons."
  (use-package all-the-icons
    :defer t))

(defun zoro-icons/post-init-neotree ()
  (setq neo-theme 'icons))

(defun zoro-icons/init-all-the-icons-dired ()
  "Initialize all-the-incons for dired."
  (use-package all-the-icons-dired
    :defer t
    :init
    (progn
      ;; TODO It seems there are some bugs.
      (defun spacemacs/delay-all-the-icons-dired-mode ()
        "Work around for ranger."
        (run-at-time 0.01 nil 'all-the-icons-dired-mode))
      (add-hook 'dired-mode-hook
                'spacemacs/delay-all-the-icons-dired-mode))
    :config
    (spacemacs|diminish all-the-icons-dired-mode)))
;;; packages.el ends here
