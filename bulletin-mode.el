(require 's)

(defvar-local bulletin-mode-marks nil
  "Marks denoting the start of each bulletin in the buffer.")

;;
;; functions for the 80-dash separator (AusCERT convention)
;; idea: could this be an emacs 'page separator' and render as a pretty horizontal line like page-break-lines-mode but still copy out as 80 dashes?
;;       would likely involve either copying from page-break-lines-mode or sneaking 80 dashes into the buffer display table there
;;       (I just read that code quickly, idk much about it)
;;       the other option is to use a ^L char directly and have it copy out as dashes, not sure which is more reliable
;;

(defun make-bulletin-separator ()
  "Return a standard string of 80 dashes to separate bulletins (plus blank lines)."
  ;; The dashes should be displayed as special syntax. (font-lock face?)
  (concat "\n\n" (make-string 80 ?-) "\n\n"))

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


(make-obsolete 'highlight-80-dashes 'make-bulletin-separator "10/01/20")


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
  "Comparable to `dashcat <(sexier-tables http://a) <(sexier-tables http://b) ...`. Also sets bulletin-mode-marks and uses a progress reporter."
  (interactive)
  (setq bulletin-mode-marks nil)
  (let* ((separator (make-bulletin-separator))
         (first-loop t)
         (progress-reporter (make-progress-reporter "Fetching advisories..." 1 (length urls))) ;; useless until we start updating it inside the url loop
         (i 0))
    (dolist (url urls)
      (unless (= i 0)(insert separator))
      (push (point-marker) bulletin-mode-marks)
      (call-process "sexier-tables" nil t nil url)
      (incf i)
      (progress-reporter-update progress-reporter i))
    (progress-reporter-done progress-reporter))
  (setq bulletin-mode-marks (nreverse bulletin-mode-marks)))
  ; might also be worth replacing /^\s+$/ with // all through the document (to avoid w3m weirdnesses like \n\n\t\n)

(defun fetch-urls-interactive ()
  "Read URLs from the minibuffer until an empty line is entered, then fetch them into the current buffer."
  (interactive)
  (fetch-urls-to-current-buffer (interactively-read-urls))
  (buttonize-buffer-with-cves (current-buffer))
  (buttonize-buffer-with-bulletin-ids (current-buffer)))

(defun fetch-urls-from-urls-buffer ()
  "Read URLs from the 'urls' buffer, and fetch them into the current buffer."
  (interactive)
  (fetch-urls-to-current-buffer (get-urls-from-urls-buffer))
  (buttonize-buffer-with-cves (current-buffer))
  (buttonize-buffer-with-bulletin-ids (current-buffer)))


;;
;; navigation functions
;;

(defun previous-bulletin (pos)
  "Searches the bulletin-mode-marks list for the latest mark before 'pos'."
  (car (reverse (seq-take-while
		     #'(lambda (elem) (< elem pos))
		     bulletin-mode-marks)))
  ;; what if bulletin-mode-marks is empty?
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
  "Searches the bulletin-mode-marks list for the first mark after 'pos'."
  (car (seq-drop-while #'(lambda (elem) (not (> elem pos))) bulletin-mode-marks)))

(defun goto-next-bulletin ()
  "Jump to the next bulletin after point."
  ;; If there isn't one, just prints an error.
  ;; This is (currently) bound to C-M-n. Its counterpart is forward-list. Maybe should be C-m-e instead, end-of-defun. In which case it should be end-of-bulletin, which would need to be tracked (or inferred from the next separator?).
  (interactive)
  (let ((destination (next-bulletin (point))))
    (if destination
        (goto-char destination)
        (message "No next bulletin"))))


;;;
;;; detection
;;;

(defun buttonize-buffer-with-cves (bufname)
  "Mark CVEs in a given buffer as hyperlinks."
  ; would be nice if they could be right-clicked (or some other option) to visit AusCERT's version instead
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

(defun buttonize-buffer-with-bulletin-ids (bufname)
  "Mark ESBs/ASBs in a given buffer as hyperlinks."
  (interactive "bBuffer to add ESB/ASB buttons to: ")
  (save-excursion
    (with-current-buffer bufname
      (goto-char (point-min))
      (while (re-search-forward "[EA]SB-[[:digit:]]\\{4\\}.[[:digit:]]\\{4\\}\\(.[[:digit:]]\\)?" nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (target-bulletin-id (buffer-substring start end)))
          (make-button start end
                       'url (format "https://auscert.org.au/bulletins/%s/" target-bulletin-id)
                       'help-echo (format "Visit %s at AusCERT" target-bulletin-id)
                       'action (lambda (button) (call-process "open" nil 0 nil (button-get button 'url)))
                       'follow-link t
                       )))))
  )

(defun set-id-title-and-buffer-name (bufname)
  "Parse the AusCERT metadata to set bulletin ID and product name, and set the buffer name based on those."
  ;; if another buffer exists with that name already, this will fail, and that condition is likely if the review process touches the same bulletin multiple times.
  (save-excursion
    (with-current-buffer bufname
      (goto-char (point-min))
      (re-search-forward "[AE]SB-[[:digit:]]\\{4\\}\\.[[:digit:]]\\{4\\}\\(\\.[[:digit:]]\\)?" nil t)
      (setq-local bulletin-id (match-string-no-properties 0))
      (goto-char (point-min))
      (re-search-forward "^Product:\s+\\(.*\\)" nil t)
      (setq-local bulletin-product (match-string-no-properties 1))
      ;; it'd be nice if product were products plural, but I'm not messing with that regex right now
      (rename-buffer (concat bulletin-id ": " bulletin-product)))
    ))

(defun detect-bulletin-separators (bufname)
  "Parse an existing bulletin to set bulletin-mode-marks."
  (interactive "bBuffer: ")
  (save-excursion
    (with-current-buffer bufname
      (setq-local bulletin-mode-marks nil)
      (goto-char (point-min))
      (re-search-forward "--------------------------BEGIN INCLUDED TEXT--------------------\n" nil t)
      (push (point-marker) bulletin-mode-marks)
      (while (re-search-forward (make-bulletin-separator) nil t)
        (push (match-end 0) bulletin-mode-marks)))
    (setq bulletin-mode-marks (nreverse bulletin-mode-marks))
    )
  bulletin-mode-marks)

;; font lock aka syntax highlighting
;; http://ergoemacs.org/emacs/elisp_font_lock_mode.html
(defcustom bulletin-mode-line-separator-face
  font-lock-warning-face
  "Face for the separator line between bulletins."
  :type 'face
  :group 'bulletin)

; build the mode's map which will be set as font-lock-defaults
(defvar bulletin-mode-highlights nil)
(setq bulletin-mode-highlights
      '(("^-\\{80\\}$" . bulletin-mode-line-separator-face)))

;;
;; core
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html
;;

(defvar bulletin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c f") 'fetch-urls-interactive)
    (define-key map (kbd "C-c g") 'fetch-urls-from-urls-buffer)
    (define-key map (kbd "C-M-p") 'goto-previous-bulletin)
    (define-key map (kbd "C-M-n") 'goto-next-bulletin)
    (define-key map (kbd "<C-tab>") 'forward-button)
    (define-key map (kbd "<C-S-tab>") 'backward-button)
    (define-key map (kbd "C-c s") 'detect-bulletin-separators)
    map))

;; much of the below is drawn from: https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html#Major-Mode-Conventions
;;;###autoload
(define-derived-mode bulletin-mode text-mode "Bulletin"
  "Create AusCERT bulletins with style."
  :group 'bulletin
  :after-hook (progn
                (goto-address-mode)
                ;; (whitespace-mode) ; for highlighting long lines: just gotta turn off showing newlines, tabs, spaces etc
                (condition-case nil ; if the rename fails, eg we have multiple buffers about the same draft, don't abort the rest of the after-hook
                    (set-id-title-and-buffer-name (current-buffer))
                  (error (message "Unable to determine bulletin ID and product from buffer contents")))
                (buttonize-buffer-with-cves (current-buffer))
                (buttonize-buffer-with-bulletin-ids (current-buffer))
                (detect-bulletin-separators (current-buffer)))
  (setq font-lock-defaults '(bulletin-mode-highlights))
)

;; features to add:
;; - detecting duplicate URLs in both entry modes
;; - interpreting an existing bulletin from text (not one created by the fetch commands)
;; - make it an emacs package/spacemacs layer and put it in source control (private github repo?)
;; fuzzy feature ideas:
;; - annotate after bulletin IDs with metadata (what kind? requires API or scraping), plus a link (easy)
;; - syntax highlighting for AusCERT preamble. not especially useful though
;; - option to open a referenced bulletin in a new buffer
;;   - HTTP and HTML parsing in elisp? or shell out/ffi to a python/rust program?
;;   - HTTPS session reuse would be nice
;; - making URLs clickable (enable goto-address-mode?)
;;   - without adding buttons to URLs containing CVE IDs (otherwise you get a different result depending whether you click the start of the URL or the bit with the CVE in it)
;;   - maybe also make C-tab and C-S-tab go to them as well as the CVE buttons
;; - clearly share functionality and distinguish between creation mode and review mode (s?). at the moment, separators in review mode don't get any special treatment (highlighting, jumping), maybe other issues too.
;; - automatically buttonize buffer when bulletins mode is activated (there's gotta be a simple way to do it, just idk it)
;; - add a second click to buttonize to make it search @ auscert's site instead of NVD
;; - include separate bulletins in the imenu (not sure how to name them, though - grab the first paragraph and join it? too long? probably would be for IBM)
;; interface to choose URLs from a block of text (i.e. ProNG's email view to pick the meaningful advisory URL from IBM's emails)
;; highlight lines past 80 width, like vim's `:colorcolumn 80'
;; - attainable with the built-in whitespace-mode: https://www.emacswiki.org/emacs/WhiteSpace
;;   - some customization required... see 'customize-group RET whitespace RET', under 'whitespace styles'
;;     - I think I want "lines, only overlong part".
;;     - also change the font face it uses to a more "error" one (it's working by setting "Whitespace line face" although the name of "Whitespace line column" looks more appropriate)
;;   - also disable most of those style options for now as I don't want to see tabs, spaces, newlines, etc (and arguably not trailing whitespace either - suspect it's more annoying than helpful)
;; for review mode: a binding to create a window layout. vertical window split, resize top window from start to "BEGIN INCLUDED TEXT", bottom window fills the rest of the space.


; ideally this regex would start with \\`, but it appears that simply cannot match in this context [dal 10/01/2020]
;;;###autoload
(add-to-list 'auto-mode-alist '("Bulletin\\.txt\\'" . bulletin-mode))

;;;###autoload
(provide 'bulletin-mode)
