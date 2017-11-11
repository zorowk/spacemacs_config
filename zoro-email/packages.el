;;; packages.el --- zoro-email layer packages file for Spacemacs.
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
;; added to `zoro-email-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `zoro-email/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `zoro-email/pre-init-PACKAGE' and/or
;;   `zoro-email/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst zoro-email-packages
  '(mu4e)
  "The list of Lisp packages required by the zoro-email layer.

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

(defun zoro-email/post-init-mu4e ()
  (progn
    (setq mu4e-account-alist
          '(("gmail"
             ;; Under each account, set the account-specific variables you want.
             (mu4e-sent-messages-behavior delete)
             (mu4e-sent-folder "/personal/[Gmail]/.Sent Mail")
             (mu4e-drafts-folder "/personal/[Gmail]/.Drafts")
             (user-mail-address "near.kingzero@gmail.com")
             (user-full-name "zorowk"))
            ("wisonic"
             (mu4e-sent-messages-behavior sent)
             (mu4e-sent-folder "/personal/Sent Mail")
             (mu4e-drafts-folder "/personal/Drafts")
             (user-mail-address "pengwenhao@wisonic.cn")
             (user-full-name "pengwenhao"))))

    ;;; Set up some common mu4e variables
    (setq mu4e-maildir "~/.mail"
          mu4e-trash-folder "/Trash"
          mu4e-refile-folder "/Archive"
          mu4e-get-mail-command "mbsync -a"
          mu4e-update-interval 60
          mu4e-compose-signature-auto-include nil
          mu4e-view-show-images t
          mu4e-view-show-addresses t)

    ;;; Mail directory shortcuts
    (setq mu4e-maildir-shortcuts
          '(("/worker/INBOX" . ?g)
            ("//INBOX" . ?c)))

    ;;; Bookmarks
    (setq mu4e-bookmarks
          `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
            ("date:today..now" "Today's messages" ?t)
            ("date:7d..now" "Last 7 days" ?w)
            ("mime:image/*" "Messages with images" ?p)
            (,(mapconcat 'identity
                         (mapcar
                          (lambda (maildir)
                            (concat "maildir:" (car maildir)))
                          mu4e-maildir-shortcuts) " OR ")
             "All inboxes" ?i)))

    (with-eval-after-load 'mu4e-alert
      ;; Enable Desktop notifications
      ;; (mu4e-alert-set-default-style 'notifications)) ; For linux
      (mu4e-alert-set-default-style 'libnotify))  ; Alternative for linux

    (setq mu4e-html2text-command "/usr/bin/w3m -T text/html")

                                        ; use msmtp
    (setq message-send-mail-function 'message-send-mail-with-sendmail)
    (setq sendmail-program "/usr/local/bin/msmtp")
                                        ; tell msmtp to choose the SMTP server according to the from field in the outgoing email
    (setq message-sendmail-extra-arguments '("--read-envelope-from"))
    (setq message-sendmail-f-is-evil 't)

    (setq message-kill-buffer-on-exit t)
   ))
;;; packages.el ends here
