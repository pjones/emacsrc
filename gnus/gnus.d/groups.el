;;; Configuration for Subscribed Groups

;; Specific Groups
(setq
 pmade-archive-group "nnimap+mail.pmade.com:Archive"
 pmade-review-group  "nnimap+mail.pmade.com:Review"
 pmade-spam-group  "nnimap+mail.pmade.com:spam"
 pmade-rebekah-group "nnimap+mail.pmade.com:Rebekah"
 pmade-trash-group "nnimap+mail.pmade.com:Deleted Messages")

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

;; Posting Styles (least specific to most specific)
(setq gnus-posting-styles
      `((".*"
         (signature-file ,(concat mail-signature-dir "/pmade")
         (From "Peter Jones <pjones@pmade.com>")))
        ;; Message from Rebekah
        ((header "from" ".*dontpatroneyesme.*")
         (gcc ,pmade-rebekah-group)
         (From "Peter Jones <pjones@pmade.com>"))
        (".*Rebekah"
         (gcc ,pmade-rebekah-group)
         (From "Peter Jones <pjones@pmade.com>"))
        ;; Messages going to the pmade.com domain
        ((header "to" "pjones@pmade\\.com")
         (signature-file ,(concat mail-signature-dir "/pmade"))
         (From "Peter Jones <pjones@pmade.com>"))
        ;; Messages going to devalot.com
        ((header "to" "pjones@devalot\\.com")
         (signature-file ,(concat mail-signature-dir "/devalot"))
         (From "Peter Jones <pjones@devalot.com>"))
        ;; Messages going to the contextualdevlopment.com domain
        ((header "to" "pjones@contextualdevelopment\\.com")
         (signature-file ,(concat mail-signature-dir "/contextual"))
         (From "Peter Jones <pjones@contextualdevelopment.com>"))
        ;; Messages to my Land Cruiser Aliases
        ((header "to" "suv8@pmade\\.org")
         (From "Peter Jones <suv8@pmade.org>")
         (signature-file ,(concat mail-signature-dir "/fj40")))
        ;; News groups
        (message-news-p
         (From "Peter Jones <mlists@pmade.com>"))
        ;; Mailing Lists
        ("Derailed"
         (To "DeRailed <derailed@googlegroups.com>")
         (From "Peter Jones <pjones@devalot.com>")
         (signature-file ,(concat mail-signature-dir "/devalot")))
        ("BoulderRuby"
         (To "Boulder Ruby Group <bdrg-members@rubyforge.org>")
         (From "Peter Jones <mlists@pmade.com>")
         (signature-file ,(concat mail-signature-dir "/devalot")))
        (".*mlists"
         (From "Peter Jones <mlists@pmade.com>"))
        (".*rb-appscript"
         (To "rb-appscript-discuss@rubyforge.org")
         (From "Peter Jones <mlists@pmade.com>"))
        (".*lcml"
         (To "lcml@tlca.org")
         (From "Peter Jones <suv8@pmade.org>")
         (signature-file ,(concat mail-signature-dir "/fj40")))
        ;; ThriveSmart INBOX
        ("^nnimap\\+Thrive Smart:.*"
         (From "Peter Jones <peter@thrivesmart.com>")
         (signature-file ,(concat mail-signature-dir "/thrivesmart")))
        ;; Aura Software INBOX
        ("^nnimap\\+Aura Software:.*"
         (From "Peter Jones <pjones@aura-software.com>")
         (signature-file ,(concat mail-signature-dir "/aurasoftware")))
        ;; SCORS Inbox
        ("^nnimap\\+SCORS:.*"
         (From "Peter Jones <peter.jones@ors.sc.gov>")
         (signature-file ,(concat mail-signature-dir "/scors")))
        ;; Google Mailing Lists
        ((header "to" ".*@googlegroups\\.com")
         (From "Peter Jones <pjones@pmade.com>"))))

;; Mailing Lists
(setq message-subscribed-addresses
      '("landcruisers@tlca.org"
        "rb-appscript-discuss@rubyforge.org"
        "bdrg-members@rubyforge.org"
        "emacs-app-dev-@lists.sourceforge.net"
        "derailed@googlegroups.com"
        "help-gnu-emacs@gnu.org"))

;; Group List
(setq
 pmade-group-sep "%9{ ⎮ %}"
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
    (gnus-topic-mode 1)
    (local-set-key "i" 'pmade-gnus-goto-inbox)))
