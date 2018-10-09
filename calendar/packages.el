;;; packages.el --- Calendar Framework Layer config for Spacemacs
;;
;; Copyright (c) 2017 sineer
;;
;; Author: Jérémie Plante <j@war.io>
;; URL: https://github.com/sineer/spacemacs-calendar-layer
;;
;; License: MIT

(defconst calendar-packages
  '(calfw
    calfw-org
    org-gcal
    alert))

(defun calendar/init-calfw ()
  "Initialize calfw and add key-bindings"
  (use-package calfw
    :defer t
    :commands (cfw:open-calendar-buffer)
    :init
    (spacemacs/set-leader-keys "aC" 'cfw:open-calendar-buffer)
    :config
    (progn
      (define-key cfw:calendar-mode-map (kbd "SPC") 'spacemacs-cmds)
      (define-key cfw:calendar-mode-map (kbd "TAB") 'cfw:show-details-command)
      (define-key cfw:calendar-mode-map (kbd "C-j") 'cfw:navi-next-item-command)
      (define-key cfw:calendar-mode-map (kbd "C-k") 'cfw:navi-prev-item-command))))

(defun calendar/init-calfw-org ()
  "Initialize calfw-org and add key-bindings"
  (use-package calfw-org
    :defer t
    :commands (cfw:open-org-calendar)
    :init
    (spacemacs/set-leader-keys "aoC" 'cfw:open-org-calendar)
    :config
    (progn
      (define-key cfw:org-schedule-map (kbd "SPC") 'spacemacs-cmds)
      (define-key cfw:org-schedule-map (kbd "TAB") 'cfw:org-open-agenda-day)
      (define-key cfw:org-custom-map (kbd "SPC") 'spacemacs-cmds)
      (define-key cfw:org-custom-map (kbd "TAB") 'cfw:org-open-agenda-day))))

(defun calendar/init-org-gcal ()
  "Initialize org-gcal"
  (use-package org-gcal
    :defer t
    :commands (org-gcal-sync
               org-gcal-fetch
               org-gcal-post-at-point
               org-gcal-delete-at-point)))

(defun calendar/init-alert ()
  "Initialize alert"
  (use-package alert
    :defer t))
