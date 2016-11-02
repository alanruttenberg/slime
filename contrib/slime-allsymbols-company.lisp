(in-package :swank)

#+abcl (require "JSS")

(defun format-allsymbols-completion-set (symbols package-name)
  "Format a set of completion strings. Returns a list of strings with package qualifiers if needed."
  (let ((this (find-package (read-from-string package-name))))
    (mapcar (lambda (symbol) 
	      (multiple-value-bind (sym internal-p) (find-symbol (string symbol) this)
		(string-downcase 
		 (if (or (eq sym symbol) (eq (symbol-package sym) (symbol-package symbol)))
		     (string symbol)
		     (if (keywordp sym)
			 (cat ":" (string symbol))
			 (untokenize-symbol (package-name (symbol-package symbol)) internal-p (string symbol)))))))
	    symbols)))

(defslimefun allsymbol-completions (string package-name)
  (let ((results nil))
    #+abcl
    ;; 3x faster
    (locally (declare (optimize (speed 3) (safety 0)))
      (let ((pattern (#"compile" 'java.util.regex.pattern (concatenate 'string "(?i)^" (#"quote" 'java.util.regex.Pattern string) ".*"))))
	(jss::with-constant-signature ((matcher "matcher") (matches "matches"))
	  (do-all-symbols (s )
	    (when (matches (matcher pattern (string s)))
	      (push s results))))))
    #-abcl
    (do-all-symbols (s)
      (when (eql (search string (string s) :test 'char-equal) 0)
	(push s results)))
    (let ((default-package (find-package package-name)))
      (flet ((relative-importance (a b)
	       (declare (ignore b))
	       (or  (eq (symbol-package a) default-package)
		    (boundp a)
		    (fboundp a)
		    (not (keywordp a)))))
	(list (format-allsymbols-completion-set 
	 (sort results #'relative-importance)
	  package-name
	  ) string)))))

