;; SMTP
(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 465
      wl-smtp-authenticate-type "login"
      wl-smtp-posting-user "username"
      wl-smtp-posting-server "smtp.exmail.qq.com"
      wl-local-domain "exmail.qq.com"
      wl-message-id-domain "smtp.exmail.qq.com")  

;; IMAP
(setq elmo-imap4-default-server "imap.exmail.qq.com"
      elmo-imap4-default-user "username@wisonic.cn"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl)

;; 送信设置
(setq wl-from "pengwenhao <username@componay.cn>"

      ;; All system folders (draft, trash, spam, etc) are placed in the
      ;; [Gmail]-folder, except inbox. "%" means it's an IMAP-folder
      wl-default-folder "%inbox"
      wl-draft-folder   "%Drafts"
      wl-trash-folder   "%Trash"
      ;; The below is not necessary when you send mail through Gmail's SMTP server,
      ;; see https://support.google.com/mail/answer/78892?hl=en&rd=1
      wl-fcc            "%Sent"

      ;; Mark sent messages as read (sent messages get sent back to you and
      ;; placed in the folder specified by wl-fcc)
      wl-fcc-force-as-read            t
      wl-draft-always-delete-myself   t
      wl-message-id-use-message-from  t
      wl-interactive-send             t
      ;; 屏幕将像普通的邮件程序一样位于3窗格中
      wl-stay-folder-window           t

      ;; For auto-completing foldernames
      wl-default-spec "%")

;; 在发送文件夹中显示汇总目的地的设定
(setq wl-summary-showto-folder-regexp "Sent")

;; 过滤掉邮件头中不需要显示的内容
(setq wl-message-ignored-field-list
  '(".*Received:" ".*Path:" ".*Id:" "^References:"
    "^Replied:" "^Errors-To:"
    "^Lines:" "^Sender:" ".*Host:" "^Xref:"
    "^Content-Type:" "^Precedence:"
    "^Status:" "^X-.*:"
    "^Content-.*:" "^MIME-Version:"
    "^Delivered-To:"
   )
 )

;; 相反你将看到的内容
(setq wl-message-visible-field-list
  '("X-Mailer.*:"
    "^X-Spam-Stat:"
    ))

;; 添加用于消息排序的标题
(setq elmo-msgdb-extra-fields
      '("X-Spam-Stat"
       ))

;; 邮件使用标题排序
(setq wl-refile-rule-alist
        '(("X-Spam-Stat"
                ("Yes" . "%Junk")
          )))

;;不要再发送时分割大量消息
(setq mime-edit-split-message nil)

;; 使用简短的邮件客户端标志
(setq wl-generate-mailer-string-function 
        'wl-generate-user-agent-string-1)

;; 信件提示
(setq wl-biff-check-folder-list '("%inbox"))

;; 显示摘要的格式， 取消线程显示默认值
(setq wl-summary-default-view 'sequence)