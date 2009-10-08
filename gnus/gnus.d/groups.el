;;; Configuration for Subscribed Groups

;; Settings by Group
(setq gnus-parameters
      '(
        ;; Defaults
        (".*INBOX.*"
         (display . all)
         (gnus-thread-sort-functions 'gnus-thread-sort-by-date))
        
        ;; Mailing Lists
        ("\\.mlists\\."
         (comment . "Mailing List")
         (display . [unread])
         (subscribed . t)
         (auto-expire . t)
         (expiry-wait . 14)
         (gnus-thread-sort-functions '(gnus-thread-sort-by-number (not gnus-thread-sort-by-date))))))
        
;; Posting Styles
(setq gnus-posting-styles
      `((".*"
         (signature-file ,(concat pmade-sigs-dir "/default")))
        (".*mlists"
         (From "Peter Jones <mlists@pmade.com>"))
        ((header "to" ".*@googlegroups\\.com")
         (From "Peter Jones <pjones@pmade.com>"))
        (message-news-p
         (From "Peter Jones <mlists@pmade.com>"))))

;; Mailing Lists
(setq message-subscribed-addresses
      '("landcruisers@tlca.org"
        "rubyosa-discuss@rubyforge.org"
        "bdrg-members@rubyforge.org"
        "emacs-app-dev-@lists.sourceforge.net"
        "derailed@googlegroups.com"
        "devalot@googlegroups.com"
        "help-gnu-emacs@gnu.org"
        "engine-developers@lists.rails-engines.org"
        "muse-el-discuss@gna.org"
        "pdf-reader@googlegroups.com"
        "emacs-orgmode@gnu.org"
        "ruby_emacs_dev@yahoogroups.com"
        "emacs-orgmode@gnu.org"
        "emacs-on-rails@googlegroups.com"))

;; Specific Groups
(setq
 pmade-archive-group "nnimap+mail.pmade.com:INBOX.Archive"
 pmade-review-group  "nnimap+mail.pmade.com:INBOX.Review"
 pmade-spam-group  "nnimap+mail.pmade.com:INBOX.spam")

;; Group List
(setq
 pmade-group-sep "%9{ ┆ %}"
 gnus-group-mode-line-format "Gnus: %%b"
 gnus-group-line-format (concat "%P" "%5{%M%S%}" pmade-group-sep "%6{%4y%}" pmade-group-sep "%*"
                                "%8{%-30,30G%}" pmade-group-sep "%1L" pmade-group-sep
                                "%6{%-22,22ud%}" pmade-group-sep "\n"))

;; Key Bindings
(defun pmade-gnus-goto-inbox ()
  (interactive)
  (goto-char (point-min))
  (search-forward "INBOX"))

;; Various Settings
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(setq gnus-newsgroup-variables '(gnus-thread-sort-functions))

(add-hook 'gnus-group-mode-hook
  (lambda ()
    (gnus-topic-mode)
    (local-set-key "i" 'pmade-gnus-goto-inbox)))
