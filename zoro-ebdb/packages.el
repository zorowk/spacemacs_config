;;; packages.el --- zoro-ebdb layer packages file for Spacemacs.
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
;; added to `zoro-ebdb-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `zoro-ebdb/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `zoro-ebdb/pre-init-PACKAGE' and/or
;;   `zoro-ebdb/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst zoro-ebdb-packages
  '((ebdb :location (recipe
                     :fetcher github
                     :repo "girzel/ebdb"))
    ebdb-i18n-chn
    company-ebdb
    pyim))

(defun zoro-ebdb/init-ebdb ()
  "Initialize alert"
  (use-package ebdb
    :defer t
    :config
    (progn
      (require 'ebdb-mua)
      (require 'ebdb-gnus)
      (require 'ebdb-com)
      (require 'ebdb-vcard)
      (require 'ebdb-complete)
      (ebdb-complete-enable)
      )))

(defun zoro-ebdb/init-ebdb-i18n-chn ()
  "Initialize alert"
  (use-package alert
    :defer t))

(defun zoro-ebdb/init-company-ebdb ()
  "Initialize alert"
  (use-package alert
    :defer t))

(defun zoro-ebdb/init-pyim ()
  "Initialize alert"
  (use-package alert
    :defer t
    :config
    (progn
      (cl-defmethod ebdb-field-search
        :around (field criterion)
        (or (cl-call-next-method)
            (when (stringp criterion)
              (let ((str (ebdb-string field)))
                (cl-some
                 (lambda (pinyin)
                   (string-match-p criterion pinyin))
                 (append (pyim-hanzi2pinyin str nil "" t)
                         (pyim-hanzi2pinyin str t "" t))))))))
    ))
;;; packages.el ends here
