(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; os x
(setq mac-command-modifier 'ctrl)
(setq mac-option-modifier 'meta)

(cua-mode 0)
(auto-fill-mode 0)
(setq fill-column 72)
(show-paren-mode 1)

(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy nil)
(setq mac-pass-command-to-system nil)

(server-start)

(add-to-list 'load-path "~/git/emacs/packages/")

;; common lisp
(require 'cl)

;; keep backup files in one place
(setq backup-directory-alist (quote ((".*" . "~/backups/"))))
(setq auto-save-file-name-transforms `((".*" ,"~/backups/" t)))

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
(defalias 'cm 'css-mode)
(defalias 'fold 'jao-toggle-selective-display)
(defalias 'jd 'insert-javascript-doc)
(defalias 'rename 'rename-file-and-buffer)

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
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-basic-offset 4)
(setq tab-width 4)
(setq standard-indent 4)
(setq sgml-basic-offset 4)

(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%R"))
)

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

(defun memories ()
  (interactive)
  (find-file "~/Dropbox/memories.txt")
  (insert "\n")
  (insert (format-time-string "%Y-%m-%d-%R"))
  (insert "\n")
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

;; code folding
;; Show-hide
(global-set-key (kbd "<f8>") 'hs-hide-block)
(global-set-key (kbd "<f9>") 'hs-show-block)
(global-set-key (kbd "<f10>") 'hs-hide-all)
(global-set-key (kbd "<f11>") 'hs-show-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MUSTACHE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mustache-mode)
;; js templating
(setq auto-mode-alist (cons '("\\.tpl$" . tpl-mode) auto-mode-alist))
(autoload 'tpl-mode "tpl-mode" "Major mode for editing CTemplate files." t)
(add-hook 'tpl-mode-hook '(lambda () (font-lock-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLOR THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/git/emacs/packages/color-theme-6.6.0")
(add-to-list 'load-path "~/git/emacs-color-theme-solarized")
(load "color-theme-6.6.0/color-theme.el")
(require 'color-theme)
(require 'color-theme-solarized)
(interactive)
(color-theme-solarized-dark)

;; html-mode
(add-hook 'html-mode-hook 'auto-fill-mode nil)
(setq auto-mode-alist (append '(("\\.html$" . html-mode))
					   auto-mode-alist))

;; org-mode
(setq auto-mode-alist (append '(("\\.notes$" . org-mode))
					   auto-mode-alist))
(setq org-startup-folded nil )
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t))
          t)

;; markdown
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))

;; dos mode
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;; shell
(add-hook 'comint-mode-hook
 (lambda ()
   (define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
   (define-key comint-mode-map (kbd "C-n") 'comint-next-input)
))

;; line numbers
(global-linum-mode 1)

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

(shell)
;; (add-hook 'after-init-hook #'(lambda ()
;;                               (split-window-vertically)
;;                               (other-window 1)
;;                               (run-scheme "you-are-here")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVASCRIPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(defalias 'jm 'js2-mode)
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

(setq auto-mode-alist (append '(("\\.json$" . js2-mode))
					   auto-mode-alist))
(setq auto-mode-alist (append '(("\\.js$" . js2-mode))
					   auto-mode-alist))
(setq javascript-indent-level 4)
;; (setq javascript-expr-indent-offset 0)
;; (setq javascript-auto-indent-flag nil)

;; autocomplete
(add-to-list 'load-path "~/git/emacs/packages/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/git/emacs/packages/auto-complete/dict/")
; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; snippets
(require 'yasnippet)
(yas/initialize)
;; Load the snippet files themselves
(yas/load-directory "~/git/emacs/packages/yasnippet/snippets/js-mode")
;; Let's have snippets in the auto-complete dropdown
(add-to-list 'ac-sources 'ac-source-yasnippet)

;; code folding
(add-hook 'js2-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
    ;; add any symbols to a buffer-local var of acceptable global vars
   (add-hook 'js2-post-parse-callbacks
          (lambda ()
            ;; strip newlines etc so the regexp below will match a multiline comment
            (let ((btext (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t)))
              (setq js2-additional-externs
                    (split-string
                     (if (string-match "/\\* *global \\(.*?\\)\\*/" btext) (match-string-no-properties 1 btext) "")
                     "[ ,]+" t))
              )))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; END JAVASCRIPT MODE
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/git/emacs/packages/magit")
(require 'magit)
