;; Extracted from Aquamacs source
;; http://aquamacs.org/

;; Use Aspell
(setq ispell-program-name "aspell")
 
;; find cocoAspell's directories automatically
(defun pmade-configure-aspell ()
  "Configure Aspell automatically if it hasn't been configured already."
  (remove-hook 'ispell-kill-ispell-hook 'pmade-configure-aspell) 
  ;; only once please
  (when (and (equal ispell-program-name "aspell")
	     ;;	     (not ispell-dictionary-alist) ;; nothing found yet
	     (not (getenv "ASPELL_CONF")))
    ;; don't do this - would assume default dirs 	
    ;;(ispell-maybe-find-aspell-dictionaries) ;; try to find dictionaries
    ;; (setq ispell-have-aspell-dictionaries nil)
    ;; to find out if it's already configured
    ;;(unless ispell-dictionary-alist
      (condition-case nil
	  (if (with-temp-buffer
	      ;; is there a stored cocoaSpell configuration? 
		(ispell-call-process ispell-program-name nil t nil "dicts")
		(eq (point-min) (point-max))) ;; no output?
	      ;; OK, aspell has not been configured by user on Unix level
	      ;; or in Emacs
	      (setenv 
	       "ASPELL_CONF"
	       (let ((config-dir (expand-file-name 
				  "~/Library/Preferences/cocoAspell"))
		     (dict-dir 
		      (car ;; use the first subdir in that path
		       (file-expand-wildcards 
			"/Library/Application Support/cocoAspell/aspell*"))))
		 ;; check if the directories are readable
		 (if (file-readable-p config-dir) 
		     (setq config-dir (concat "conf-dir " config-dir))
		   (setq config-dir nil))
		 (if (file-readable-p dict-dir) 
		     (setq dict-dir (concat ";dict-dir " dict-dir))
		   (setq dict-dir nil))
		 (concat config-dir dict-dir))))
	(error nil))))

(add-hook 'ispell-kill-ispell-hook 'pmade-configure-aspell)
