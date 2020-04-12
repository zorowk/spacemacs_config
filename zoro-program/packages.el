;;; packages.el --- zoro-program layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: zoro <zoro@zoro-hp>
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
;; added to `zoro-program-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `zoro-program/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `zoro-program/pre-init-PACKAGE' and/or
;;   `zoro-program/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst zoro-program-packages
  '(
    cmake-font-lock
    cmake-mode
    lsp-mode
    company-box
    company
    (company-tabnine :requires company)
    (python :location built-in)
    (emacs-lisp :location built-in)
    )
  "The list of Lisp packages required by the zoro-program layer.

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


;;; packages.el ends here
(defun zoro-program/post-init-lsp-mode ()
  (with-eval-after-load 'lsp-mode
    (advice-add 'lsp :after #'zorowk/merge-company-tabnine-to-company-lsp))

  (progn
    (setq lsp-ui-doc-enable nil)

    (defun lsp--auto-configure ()
      "Autoconfigure `lsp-ui', `company-lsp' if they are installed."

      (with-no-warnings
        (when (functionp 'lsp-ui-mode)
          (lsp-ui-mode))

        (cond
         ((eq :none lsp-prefer-flymake))
         ((and (not (version< emacs-version "26.1")) lsp-prefer-flymake)
          (lsp--flymake-setup))
         ((and (functionp 'lsp-ui-mode) (featurep 'flycheck))
          (require 'lsp-ui-flycheck)
          (lsp-ui-flycheck-enable t)
          (flycheck-mode 1)))

        (when (functionp 'company-lsp)
          (company-mode 1)

          ;; make sure that company-capf is disabled since it is not indented to be
          ;; used in combination with lsp-mode (see #884)
          (setq-local company-backends (remove 'company-capf company-backends))

          (when (functionp 'yas-minor-mode)
            (yas-minor-mode t)))))

    (add-hook 'lsp-after-open-hook 'zorowk-refresh-imenu-index)

    (defun hidden-lsp-ui-sideline ()
      (interactive)
      (if (< (window-width) 180)
          (progn
            (setq lsp-ui-sideline-show-code-actions nil)
            (setq lsp-ui-sideline-show-diagnostics nil)
            (setq lsp-ui-sideline-show-hover nil)
            (setq lsp-ui-sideline-show-symbol nil))
        (progn
          ;; (setq lsp-ui-sideline-show-code-actions t)
          ;; (setq lsp-ui-sideline-show-diagnostics t)
          (setq lsp-ui-sideline-show-hover t)
          ;; (setq lsp-ui-sideline-show-symbol t)
          )))

    (advice-add 'lsp-ui-sideline--run :after 'hidden-lsp-ui-sideline)

    (setq lsp-auto-configure t)
    (setq lsp-prefer-flymake nil)))

(defun zoro-program/post-init-emacs-lisp ()
  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

(defun zoro-program/post-init-python ()
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w"))))

(defun zoro-program/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

(defun zoro-program/post-init-cmake-mode ()
  (progn
    (spacemacs/declare-prefix-for-mode 'cmake-mode
      "mh" "docs")
    (spacemacs/set-leader-keys-for-major-mode 'cmake-mode
      "hd" 'cmake-help)
    (add-hook 'cmake-mode-hook (function cmake-rename-buffer))))

(defun zoro-program/post-init-company-box ()
  (spacemacs|use-package-add-hook company-box
    :post-config
    (progn
      (push #'zorowk/company-box-icons--tabnine
            company-box-icons-functions)
      (map-put! company-box-backends-colors
               'company-tabnine  '(:all
                                   tabnine-company-box-backend-tabnine-face
                                   :selected
                                   tabnine-company-box-backend-tabnine-selected-face))
      )
    )
  )

(defun zoro-program/post-init-company ()
  (unless (configuration-layer/layer-used-p 'lsp)
    (with-eval-after-load 'company
      (push #'company-tabnine company-backends)))
  )

(defun zoro-program/init-company-tabnine ()
  (use-package company-tabnine
    :defer t
    :init
    :config
    (progn
      (setq company-tabnine-max-num-results 3)

      (add-to-list 'company-transformers 'zorowk/sort-by-tabnine t)
      ;; The free version of TabNine is good enough,
      ;; and below code is recommended that TabNine not always
      ;; prompt me to purchase a paid version in a large project.
      (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
        (let ((company-message-func (ad-get-arg 0)))
          (when (and company-message-func
                   (stringp (funcall company-message-func)))
            (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
              ad-do-it))))
      ))
  )
