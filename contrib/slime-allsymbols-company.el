(require 'slime)
(require 'cl-lib)

;; based on slime-company

(define-slime-contrib slime-allsymbols-company
  "Use slime-company completion over all packages, and dabbrev-expand for tab completion"
  (:license "GPL")
  (:authors "Alan Ruttenberg")
  (:slime-dependencies slime-company)
  (:swank-dependencies slime-allsymbols-company)
  (:on-load (slime-allsymbols-company--enable))
  (:on-unload (slime-allsymbols-company--disable))
  )

;; doesn't like to be calle directly
(defun dabbrev-expand-complete (&optional arg)
  (interactive "*P")
  (call-interactively 'dabbrev-expand))
    
(defun slime-allsymbols-company--enable ()
  (setq slime-completion-at-point-functions (subst 'dabbrev-expand-complete 'slime-simple-completion-at-point slime-completion-at-point-functions))
  (setq company-backends (remove 'company-slime company-backends))
  (setq company-backends (cons 'company-slime-allsymbols company-backends))
  (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
    (remove-hook h 'slime-company-maybe-enable)
    (add-hook h 'slime-allsymbols-company-maybe-enable)))

(defun slime-allsymbols-company--disable ()
  (setq company-backends (remove 'company-slime-allsymbols company-backends))
  (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
    (remove-hook h 'slime-allsymbols-company-maybe-enable)))

(defun slime-allsymbols-company-maybe-enable ()
  (when (slime-company-active-p)
    (company-mode 1)
    (add-to-list 'company-backends 'company-slime-allsymbols)
    (setq slime-company-completion 'allsymbol-complete)))

(defun slime-company-disable ()
  (setq company-backends (remove 'company-slime company-backends)))

(defun slime-allsymbols-company--fetch-candidates-async (prefix)
  (when (slime-connected-p)
    (let ((slime-current-thread t))
      (lexical-let ((package (slime-current-package))
		    (prefix prefix))
	(cons :async (lambda (callback)
		       (lexical-let ((callback callback))
			 (slime-eval-async
			     `(swank:allsymbol-completions ,prefix ',package)
			   (lambda (result)
			     (funcall callback (car result))))
			 package)))))))

(defun company-slime-allsymbols (command &optional arg &rest ignored)
  "Company mode backend for slime."
  (cl-case command
	   (init
	    (slime-company-active-p))
	   (prefix
	    (when (and (slime-company-active-p)
		       (slime-connected-p)
		       (or slime-company-complete-in-comments-and-strings
			   (null (company-in-string-or-comment))))
	      (company-grab-symbol)))
	   (candidates
	    (slime-allsymbols-company--fetch-candidates-async (substring-no-properties arg)))
	   (match
	       (let ((prefix (company-grab-symbol) ))
		 (+ (search prefix arg) (length prefix))))
	   (meta
	    (slime-company--arglist (substring-no-properties arg)))
	   (annotation (concat " " (get-text-property 0 'flags arg)))
	   (doc-buffer
	    (slime-company--doc-buffer (substring-no-properties arg)))
	   (location
	    (slime-company--location (substring-no-properties arg)))
	   (post-completion
	    (slime-company--post-completion (substring-no-properties arg)))
	   (no-cache t)
	   (sorted t)))

(provide 'slime-allsymbols-company)
