;;; Code:
(require 'mu4e)
;; (require 'org-mime)
;; (require 'org-mu4e)
(require 'smtpmail)
(require 'mu4e-alert)

(setq personal-email (password-store-get-field "mbsync/personal-gmail" "username"))

(setq

 ;; get mail options
 mu4e-get-mail-command "mbsync -a"
 mu4e-update-interval 120
 mu4e-headers-auto-update t
 mu4e-view-prefer-html t

 ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
 ;; Override in context switching for other type of mailboxes
 mu4e-sent-messages-behavior 'delete
 message-kill-buffer-on-exit t

 shr-color-visible-luminance-min 40

 mu4e-view-show-images t
 mu4e-use-fancy-chars t

 ;; This fixes the error 'mbsync error: UID is x beyond highest assigned UID x'
 mu4e-change-filenames-when-moving t

 user-mail-address personal-email
 user-full-name "Benj Bellon"

 ;; default to first (personal) context
 mu4e-context-policy 'pick-first
 mu4e-compose-context-policy 'pick-first

 ;; Customize the flags to something more sensible.
 mu4e-headers-flagged-mark   '("F" . "⚑")
 mu4e-headers-unread-mark '("u" . "✉")
 mu4e-headers-replied-mark   '("R" . "←")

 mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-attachment-dir "~/Downloads/.mail-attachments"
 mu4e-maildir "~/.mail/"

 ;; sendmail options
 message-send-mail-function   'smtpmail-send-it
 smtpmail-debug-info t
 smtpmail-stream-type 'starttls
 smtpmail-default-smtp-server "smtp.gmail.com")

;; This will ensure the right 'sent from' address and email sign off etc. be
;; picked up when replying to emails.
(setq mu4e-contexts
      `(
        ,(make-mu4e-context
          :name "personal"
          :enter-func (lambda () (mu4e-message "Entering personal account context"))
          :leave-func (lambda () (mu4e-message "Leaving personal account context"))
          ;; We match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg :to personal-email)
                          (mu4e-message-contact-field-matches msg :from personal-email)
                          (mu4e-message-contact-field-matches msg :cc personal-email)
                          (mu4e-message-contact-field-matches msg :bcc personal-email)))
          :vars `((user-mail-address . ,personal-email)
                  (user-full-name . "Benj Bellon")

                  (mu4e-drafts-folder . "/personal-gmail/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/personal-gmail/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/personal-gmail/[Gmail]/Trash")
                  (mu4e-refile-folder . "/personal-gmail/[Gmail]/All Mail")

                  (mu4e-maildir-shortcuts . (("/personal-gmail/Inbox" . ?i)
                                             ("/personal-gmail/[Gmail]/Drafts" . ?d)
                                             ("/personal-gmail/[Gmail]/Trash" . ?t)
                                             ("/personal-gmail/[Gmail]/All Mail" . ?a)))

                  (mu4e-bookmarks . (("date:today..now AND flag:unread AND NOT (\"maildir:/personal-gmail/[Gmail]/Trash\")" "Inbox" ?i)
                                     ("date:today..now AND NOT (\"maildir:/personal-gmail/[Gmail]/Trash\" OR \"maildir:/personal-gmail/[Gmail]/Sent Mail\")" "Today" ?t)
                                     ("date:7d..now AND NOT (\"maildir:/personal-gmail/[Gmail]/Trash\" OR \"maildir:/personal-gmail/[Gmail]/Sent Mail\")" "Last 7 Days" ?w)
                                     ("NOT (\"maildir:/personal-gmail/[Gmail]/Trash\" OR \"maildir:/personal-gmail/[Gmail]/Sent Mail\")" "All" ?a)))

                  (smtpmail-queue-dir . "~/.mail/personal-gmail/queue/cur")
                  (smtpmail-smtp-user . ,personal-email)
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)))
        ))


(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(add-to-list 'mu4e-view-actions '("View In Browser" . mu4e-action-view-in-browser) t)

(add-hook 'mu4e-compose-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'message-send-hook (lambda () (mml-secure-message-sign-pgpmime)))

;; mu4e-alerts
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(setq
 mu4e-alert-interesting-mail-query (concat
                                    "date:today..now"
                                    " AND flag:unread"
                                    "  AND NOT (\"maildir:/personal-gmail/[Gmail]/Trash\" OR \"maildir:/personal-gmail/[Gmail]/Sent Mail\")"))


(provide 'setup-mu4e)
;;; setup-mu4e.el ends here
