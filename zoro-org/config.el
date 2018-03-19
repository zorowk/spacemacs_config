;;; config.el --- zorowk Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 zorowk
;;
;; Author: zorowk <near.kingzero@gmail.com>
;; URL: https://github.com/zorowk/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(setq-default
 org-agenda-dir "~/Documents/Worker"
 deft-dir "~/Documents/Worker"
 journal-dir "~/Documents/journal")

(defun zorowk/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(add-hook 'org-mode-hook #'zorowk/org-ispell) 
