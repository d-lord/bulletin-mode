(defvar-local bulletins-mode-marks nil
  "Marks denoting the start of each bulletin in the buffer.")

;;
;; functions for the 80-dash separator (AusCERT convention)
;;

(defun highlight-80-dashes (&optional color)
  "Set a line of exactly 80 dashes to be highlighted."
  (interactive)
;  (hi-lock-mode 1)
  (highlight-regexp "^-\\{80\\}$" (or color 'hi-yellow)))
;; Here's the problem with this: the highlight-regexp command only highlights existing matches.
;; Any matches created after this command is invoked will not be highlighted.
;; So... either find a way to ask it to detect them constantly (like vim does), or re-run this after the inserts?

(defun unhighlight-80-dashes ()
  "Undo highlight-80-dashes."
  (unhighlight-regexp "^-\\{80\\}$"))

(defun make-bulletin-separator ()
  "Return a propertized string to separate bulletins (including blank lines)."
  ;; May have replaced the (un)?highlight-80-dashes functions.
  (concat "\n\n"
	  (propertize (make-string 80 ?-)
		      'face 'hi-yellow)
	  ;; can choose other faces (colours) with M-x list-faces-display
	  "\n\n"))

;;
;; s-trim* functions borrowed, in not quite strict compliance with GPL 3, from:
;; https://github.com/magnars/s.el/
;;
(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))
(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (save-match-data
    (declare (pure t) (side-effect-free t))
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))
(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (declare (pure t) (side-effect-free t))
(s-trim-left (s-trim-right s)))

;;
;; fetching functions
;;

(defun get-urls-from-urls-buffer ()
  "Read line-separated URLs from the buffer named 'urls'."
  (with-current-buffer "urls" (mapcar #'s-trim (split-string (buffer-string) "\n" t))))

(defun interactively-read-urls ()
  "Interactively read URLs until an empty line is entered (but not EOF).
   https://www.reddit.com/r/emacs/comments/6lzpre/use_interactive_list_readstring_to_read_arbitrary/djxuzst"
  (let (urls url done)
    (while (not done)
      (setq url (read-string "URL (empty to finish): "))
      (if (= (length url) 0)
	  (setq done t)
	(push url urls)))
    (reverse urls)))

(defun fetch-urls-to-current-buffer (urls)
  "Comparable to `dashcat <(sexier-tables http://a) <(sexier-tables http://b) ...`. Also sets bulletins-mode-marks and uses a progress reporter."
  (interactive)
  (setq bulletins-mode-marks nil)
  (let* ((separator (make-bulletin-separator))
	(first-loop t)
	(progress-reporter (make-progress-reporter "Fetching advisories..." 1 (length urls)))) ;; useless until we start updating it inside the url loop
    (dolist (url urls)
      (unless first-loop (insert separator))
      (push (point-marker) bulletins-mode-marks)
      (call-process "sexier-tables" nil t nil url)
      (setq first-loop nil)
      )
    (progress-reporter-done progress-reporter))
    (setq bulletins-mode-marks (nreverse bulletins-mode-marks)))

(defun fetch-urls-interactive ()
  "Read URLs from the minibuffer until an empty line is entered, then fetch them into the current buffer."
  (interactive)
  (fetch-urls-to-current-buffer (interactively-read-urls))
  (buttonize-buffer-with-cves (current-buffer)))

(defun fetch-urls-from-urls-buffer ()
  "Read URLs from the 'urls' buffer, and fetch them into the current buffer."
  (interactive)
  (fetch-urls-to-current-buffer (get-urls-from-urls-buffer))
  (buttonize-buffer-with-cves (current-buffer)))


;;
;; navigation functions
;;

(defun previous-bulletin (pos)
  "Searches the bulletins-mode-marks list for the latest mark before 'pos'."
  (car (reverse (seq-take-while
		     #'(lambda (elem) (< elem pos))
		     bulletins-mode-marks)))
  ;; what if bulletins-mode-marks is empty?
  )

(defun goto-previous-bulletin ()
  "Jump to the nearest bulletin before point."
  (interactive)
  (let ((destination (previous-bulletin (point))))
    (if destination
	(goto-char destination)
      ;; or go to: first bulletin? point? point-min?
      (message "No previous bulletin"))))

(defun next-bulletin (pos)
  "Searches the bulletins-mode-marks list for the first mark after 'pos'."
  (car (seq-drop-while #'(lambda (elem) (not (> elem pos))) bulletins-mode-marks)))

(defun goto-next-bulletin ()
  "Jump to the next bulletin after point."
  ;; If there isn't one, just prints an error.
  ;; This is (currently) bound to C-M-n. Its counterpart is forward-list. Maybe should be C-m-e instead, end-of-defun. In which case it should be end-of-bulletin, which would need to be tracked (or inferred from the next separator?).
  (interactive)
  (let ((destination (next-bulletin (point))))
    (if destination
	(goto-char destination)
        (message "No next bulletin"))))

(defun buttonize-buffer-with-cves (bufname)
  "Mark CVEs in a given buffer as hyperlinks."
  (interactive "bBuffer to add CVE buttons to: ")
  (save-excursion
    (with-current-buffer bufname
      (goto-char (point-min))
      (while (re-search-forward "CVE-[[:digit:]]\\{4\\}-[[:digit:]]\\{4,\\}" nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (cve (buffer-substring start end)))
          (make-button start end
                       'url (format "https://nvd.nist.gov/vuln/detail/%s" cve)
                       'help-echo (format "Visit %s at NVD" cve)
                       'action (lambda (button) (call-process "open" nil 0 nil (button-get button 'url)))
                       'follow-link t
                       ))))))


;;;###autoload
(define-minor-mode bulletins-mode
  "Create AusCERT bulletins with style."
  :init-value nil
  :lighter " Bulletin"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c f") 'fetch-urls-interactive)
	    (define-key map (kbd "C-c g") 'fetch-urls-from-urls-buffer)
	    (define-key map (kbd "C-c r") (lambda () "Run 80-dash line highlighting." (interactive) (progn (unhighlight-80-dashes) (highlight-80-dashes)))) ; annoying constraints this addresses: highlighting is done once, not constantly; existing highlighting must be cleared before running highlighting will do anything. (is Hi-Lock just screwy?) But does have the problem that you can insert at the start of the line to get extra highlighting (the contamination spreads!).
	    ;; Not sure these two are exactly like their counterparts in other modes, but let's give it a go anyway.
	    (define-key map (kbd "C-M-p") 'goto-previous-bulletin)
	    (define-key map (kbd "C-M-n") 'goto-next-bulletin)
	    map)
  )
;; features to add:
;; - detecting duplicate URLs in both entry modes
;; - interpreting an existing bulletin from text (not one created by the fetch commands)
;; - make it an emacs package/spacemacs layer and put it in source control (private github repo?)
;; fuzzy feature ideas:
;; - annotate after bulletin IDs with metadata (what kind?), plus a link
;; - syntax highlighting for AusCERT preamble. not especially useful though
;; - option to open a referenced bulletin in a new buffer
;;   - HTTP and HTML parsing in elisp? or shell out/ffi to a python/rust program?
;;   - HTTPS session reuse would be nice

;;;###autoload


(make-obsolete 'highlight-80-dashes 'make-bulletin-separator)


(provide 'bulletins-mode)
