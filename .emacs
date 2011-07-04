(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; os x
(setq mac-option-modifier 'meta)

(cua-mode 0)
(auto-fill-mode 1)
(setq fill-column 72)
(show-paren-mode 1)

(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy nil)

(server-start)

(add-to-list 'load-path "~/git/emacs/packages/")
(add-to-list 'load-path "~/git/emacs/packages/color-theme-6.6.0")

;; common lisp
(require 'cl)

;; keep backup files in one place
(setq backup-directory-alist (quote ((".*" . "~/backups/"))))
(setq auto-save-file-name-transforms `((".*" ,"~/backups/" t)))

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)

;; another way for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Backwords kill
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; ;; die scrollbar
;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; ;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'ddqrr 'dired-do-query-replace-regexp)
(defalias 'rb 'revert-buffer)
(defalias 'cr 'comment-region)
(defalias 'uc 'uncomment-region)
(defalias 'gl 'goto-line)
(defalias 'rr 'replace-regexp)
(defalias 'hm 'html-mode)
(defalias 'jm 'js-mode)
(defalias 'cm 'css-mode)
(defalias 'fold 'jao-toggle-selective-display)
(defalias 'jd 'insert-javascript-doc)

;; abbrevs always on
(setq abbrev-mode t)
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
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname)		(set-buffer-modified-p nil)		t))))

;; tabs and spaces
;; use tabs
(setq indent-tabs-mode t)
;; tab key goes over 4
(setq c-basic-offset 4)
;; interpret tab to be 4 characters wide, and tab stops 4 wide
(setq tab-width 4)
(setq standard-indent 4)
(setq sgml-basic-offset 4)

(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%R")))

(defun etnotes ()
  (interactive)
  (find-file "~/Dropbox/et/todo.notes")
)

(defun todo ()
  (interactive)
  (find-file "~/Dropbox/org/tasks.notes")
)

(defun reg ()
  (interactive)
  (find-file "~/git/emacs/regex")
)

;; put in js comment structure
(defun insert-javascript-doc ()
  (interactive)
  (insert "\n")
  (insert "\n/**")
  (insert "\n * ")
  (insert "\n * @param {}")
  (insert "\n */")
  (previous-line 1)
  (end-of-line)
  (backward-char )
)

(defun emacs ()
  (interactive)
  (find-file "~/git/emacs/.emacs")
)

;; clear up files before saving them
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file and leaves single newline character."
  (interactive)
  (save-excursion
	(goto-char (point-max))
	(newline)			   ;; ensures that there is at least one
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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-autoface-mode nil)
 '(cursor-type (quote box) t))

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; html-mode
(add-hook 'html-mode-hook 'auto-fill-mode nil)
(setq auto-mode-alist (append '(("\\.html$" . html-mode))
					   auto-mode-alist))

;; org-mode
(setq auto-mode-alist (append '(("\\.notes$" . org-mode))
					   auto-mode-alist))
(setq org-startup-folded nil )

;; dos mode
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;; shell
(add-hook 'comint-mode-hook
 (lambda ()
   (define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
   (define-key comint-mode-map (kbd "C-n") 'comint-next-input)
))

;; js
(setq auto-mode-alist (append '(("\\.json$" . js-mode))
					   auto-mode-alist))
(setq javascript-indent-level 4)
(setq javascript-expr-indent-offset 0)
(setq javascript-auto-indent-flag nil)

;; js templating
(setq auto-mode-alist (cons '("\\.tpl$" . tpl-mode) auto-mode-alist))
(autoload 'tpl-mode "tpl-mode" "Major mode for editing CTemplate files." t)
(add-hook 'tpl-mode-hook '(lambda () (font-lock-mode 1)))

;; line numbers
(global-linum-mode 1)

;; code folding
(defun jao-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))
(global-set-key [f1] 'jao-toggle-selective-display)

;; revery buffer
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;; title
(setq frame-title-format
	  '(:eval
		(if buffer-file-name
			 (concat (file-name-directory buffer-file-name) "%b"))
		  (buffer-name)
		  ))

;; scroll while centered
(defun scroll-up-one-line()
  (interactive)
  (scroll-up 1))

(defun scroll-down-one-line()
  (interactive)
  (scroll-down 1))

(global-set-key [?\C-.] 'scroll-down-one-line)
(global-set-key [?\C-,] 'scroll-up-one-line)

(split-window-horizontally)

(setq save-place-file "~/.emacs.d/saveplace")
(setq save-place t)
(require 'saveplace)

;; aspx files
;; (autoload 'aspx-mode "aspx-mode.el" "Edit ASPX files." t)
;; (setq auto-mode-alist (append '(("\\.aspx$" . aspx-mode))
;; 					   auto-mode-alist))

;; file names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; auto-byte-compile
(add-hook 'emacs-lisp-mode-hook '(lambda ()
  (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))
)

;; recent files
(require 'recentf)
(recentf-mode 1)
