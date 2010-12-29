(add-to-list 'load-path "~/repo/emacs/packages/")
(add-to-list 'load-path "~/repo/emacs/packages/color-theme-6.6.0")

(setq mac-command-modifier 'meta)
(setq x-select-enable-clipboard t)

;; (setq font-lock-support-mode nil)

;; server
(server-start)

;; common lisp
(require 'cl)

;; Can't stand this in Aquaemacs
;;(cua-mode 0)

;; keep backup files in one place
(setq backup-directory-alist (quote ((".*" . "~/backups/"))))
(setq auto-save-file-name-transforms `((".*" ,"~/backups/" t)))

;; fonts
(set-default-font "-apple-inconsolata-medium-r-normal--14-*-*-*-*-*-iso10646-1")

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)

(require 'org-install)
(autoload 'org-mode "org-mode" "Org mode." t)
(setq auto-mode-alist (append '(("\\.notes$" . org-mode))
                       auto-mode-alist))
(add-hook 'org-mode-hook 'visual-line-mode)

;; another way for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; backwords kill
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; die scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'ddqrr 'dired-do-query-replace-regexp)
(defalias 'rb 'revert-buffer)
(defalias 'cr 'comment-region)
(defalias 'uc 'uncomment-region)
(defalias 'gl 'goto-line)
(defalias 'rr 'replace-regexp)
(defalias 'hm 'html-mode)
(defalias 'fold 'jao-toggle-selective-display)

;; abbrevs always on
(setq-default abbrev-mode t)
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)

;; steve yegge
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t))))

(defun image-linkify ()
  "Replace a path to image file with a HTML img tag.
   Example, if cursor is on the word “emacs_logo.png”, then it will became
   “<img src=\"emacs_logo.png\" alt=\"emacs logo\" width=\"123\" height=\"456\">”."
  (interactive)
  (let
    (img-file-path bounds img-dim width height altText myResult)
    (setq img-file-path (thing-at-point 'filename))
    (setq bounds (bounds-of-thing-at-point 'filename))
    (setq altText (file-name-nondirectory img-file-path))
    (setq altText (replace-regexp-in-string "\\.[A-Za-z]\\{3,4\\}$" "" altText t t))
    (setq altText (replace-regexp-in-string "_" " " altText t t))
    (setq img-dim (get-image-dimensions img-file-path))
    (setq width (number-to-string (car img-dim)))
    (setq height (number-to-string (car (last img-dim))))
    (setq myResult (concat "<img"
                           " alt=\"" altText "\""
                           " height=\"" height "\""
                           " src=\"" img-file-path "\""
                           " width=\"" width "\""
                           " />"))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (insert myResult))
    ))


(defun get-image-dimensions (img-file-relative-path)
  "Returns a image file's width and height as a list."
  (let (tmp dimen)
    (clear-image-cache)
    (setq tmp
          (create-image (concat default-directory img-file-relative-path)))
    (setq dimen (image-size tmp t))
    (list (car dimen) (cdr dimen))
  )
)

(global-set-key (kbd "<f5>") 'image-linkify)

(defun wrap-text (aa bb)
  "Wrap strings aa and bb around current word or region."
  (save-excursion
    (let (p1 p2 myword)
      (if (and transient-mark-mode mark-active)
          (progn (setq p1 (region-beginning)) (setq p2 (region-end)))
        (progn
          (skip-chars-backward "-A-Za-z")
          (setq p1 (point))
          (skip-chars-forward "-A-Za-z")
          (setq p2 (point))))
      (setq myword (buffer-substring-no-properties p1 p2))
      (goto-char p2) (insert bb)
      (goto-char p1) (insert aa))))

(defun wrap-span-xnt ()
  "Wrap a HTML <span> tag around current word or region."
  (interactive)
  (wrap-text "<span class=\"\">" "</span>"))

(global-set-key (kbd "<f6>") 'wrap-span-xnt)

;; tabs and spaces
;; always use spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; tab key goes over 4
(setq c-basic-offset 4)
;; interpret tab to be 4 characters wide, and tab stops 4 wide
(setq-default tab-width 4)
(setq-default standard-indent 4)

;; Fonts are automatically highlighted. For more information
;; type M-x describe-mode font-lock-mode
(global-font-lock-mode t)
(set-mouse-color "black")

;; Keep Emacs from executing file local variables.
;; (this is also in the site-init.el file loaded at emacs dump time.)
;; (setq inhibit-local-variables t ; v18
;;       enable-local-variables nil ; v19
;;       enable-local-eval nil) ; v19

;; This function is used in various programming language mode hooks below. It
;; does indentation after every newline when writing a program.
(defun newline-indents ()
  "Bind Return to `newline-and-indent' in the local keymap."
  (local-set-key "\C-m" 'newline-and-indent))

;; Tell Emacs to use the function above in certain editing modes.
(add-hook 'lisp-mode-hook (function newline-indents))
(add-hook 'emacs-lisp-mode-hook (function newline-indents))
(add-hook 'lisp-interaction-mode-hook (function newline-indents))
(add-hook 'scheme-mode-hook (function newline-indents))
(add-hook 'c-mode-hook (function newline-indents))
(add-hook 'c++-mode-hook (function newline-indents))
(add-hook 'html-mode-hook (function newline-indents))
(add-hook 'xhtml-mode-hook (function newline-indents))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(sgml-basic-offset 4)
 '(show-paren-mode t)
 '(tidy-config-file "~/.tidyrc")
 '(transient-mark-mode t)
 '(vc-follow-symlinks t)
)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode ((((class color) (min-colors 8)) nil))))

(put 'upcase-region 'disabled nil)

(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%R")))

(defun etnotes ()
  (interactive)
  (find-file "~/Dropbox/et/et.notes")
  (end-of-buffer)
)

(defun reg ()
  (interactive)
  (find-file "~/repo/emacs/regex")
)

;; clear up files before saving them
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file and leaves single newline character."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (newline)              ;; ensures that there is at least one
    (delete-blank-lines))) ;; leaves at most one

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-blank-lines)

(load "color-theme-6.6.0/color-theme.el")
(require 'color-theme)
(load "zenburn.el")
(require 'zenburn)
(color-theme-zenburn)
(eval-after-load 'mumamo
  '(eval-after-load 'zenburn
     '(ignore-errors (set-face-background
                      'mumamo-background-chunk-submode1 "#4F3F3F")
                     (set-face-background
                      'mumamo-background-chunk-submode2 "#3F4F3F")
                     (set-face-background
                      'mumamo-background-chunk-submode3 "#3F3F4F"))))
