(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

(setq-default indent-tabs-mode t)
(setq default-tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
(setq indent-line-function 'insert-tab)

(cua-mode 0)

(add-to-list 'load-path "/Users/splumlee/git/emacs/packages")
(require 'markdown-mode);

;; (add-to-list 'load-path "/Users/splumlee/git/smartparens")
;; (add-to-list 'load-path "/Users/splumlee/git/dash.el")
;; (require 'smartparens);
;; (smartparens-global-mode 1)

(show-paren-mode t)

;; use ; to start abbrevs, so change meaning in syntax table
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)

(global-auto-revert-mode t)

(modify-syntax-entry ?; "w")

(add-to-list 'load-path "/Users/splumlee/git/emacs/packages/")

;; os x
(setq mac-command-modifier 'ctrl)
(setq mac-pass-command-to-system nil)
(setq mac-option-modifier 'meta)

;; environment
;; must have paths to jshint and node for emacs-flymake
(setenv "PATH" (concat "/Users/splumlee/git/homebrew/bin:" "/Users/splumlee/git/homebrew/share/npm/lib/node_modules/jshint/bin:" "/Users/splumlee/git/homebrew/share/npm/bin:" (getenv "PATH")))
(setenv "NODE_PATH" (concat "/Users/splumlee/git/homebrew/bin/node" (concat (getenv "NODE_PATH"))))
(setq exec-path
	  '(
	"/Users/splumlee/git/homebrew/bin" ":"
	"/Users/splumlee/git/homebrew/share/npm/bin/" ":"
	))

;; various tools and pieces
;; keep backup files in one place
(setq backup-directory-alist (quote ((".*" . "/Users/splumlee/backups/" ))))
(setq auto-save-file-name-transforms `((".*", "/Users/splumlee/backups/" t)))

;; got used to this in terminal
(global-set-key (kbd "C--") 'undo)

;; another way for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Backwords kill
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; ;; die scrollbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; ;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'ddqrr 'dired-do-query-replace-regexp)
(defalias 'rb 'revert-buffer)
(defalias 'cr 'comment-region)
(defalias 'uc 'uncomment-region)
(defalias 'gl 'goto-line)
(defalias 'rr 'replace-regexp)
(defalias 'hm 'html-mode)
(defalias 'fold 'jao-toggle-selective-display)
(defalias 'rename 'rename-file-and-buffer)
(defalias 'fe 'flymake-display-err-menu-for-current-line)

;; keep what's on disk in the buffers
;; should prevent bad habits too
(global-auto-revert-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq special-display-buffer-names
	  '("*shell*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; palpimset
(add-to-list 'load-path "/Users/splumlee/git/Palimpsest/")
(require 'palimpsest);

;; git-gutter
(add-to-list 'load-path "/Users/splumlee/git/emacs-git-gutter/")
(require 'git-gutter)
;; If you enable global minor mode
(global-git-gutter-mode nil)
;; If you enable git-gutter-mode for some modes
(add-hook 'js3-mode-hook 'git-gutter-mode)
;; bind git-gutter toggle command
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
;; Jump to next/previous diff
(global-set-key (kbd "C-x p") 'git-gutter:previous-diff)
(global-set-key (kbd "C-x n") 'git-gutter:next-diff)
;; ignore all spaces
(setq git-gutter:diff-option "-w")

;; flymake mode
;; 
(add-to-list 'load-path "/Users/splumlee/git/emacs-flymake/")
(require 'flymake);
;; (setq flymake-log-level 3)
;; https://github.com/illusori/emacs-flymake
(add-to-list 'load-path "/Users/splumlee/git/emacs-flymake-cursor/")
(add-hook 'find-file-hook 'flymake-find-file-hook)
;; https://github.com/illusori/emacs-flymake-cursor
(eval-after-load 'flymake '(require 'flymake-cursor))
;; (eval-after-load 'flymake 
;;   '(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;; 	 (setq flymake-check-was-interrupted t))
;;   (ad-activate 'flymake-post-syntax-check))

(setq flymake-max-parallel-syntax-checks 8)
(setq flymake-run-in-place nil)
(setq temporary-file-directory "/tmp")

;; desktop mode
(desktop-save-mode 1)
(setq desktop-path '("/Users/splumlee/Dropbox"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; abbrevs always on
(read-abbrev-file "/Users/splumlee/.abbrev_defs")
(setq abbrev-mode t)
(setq save-abbrevs t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(require 'package)
(add-to-list 'package-archives
		 '("marmalade" . "http://marmalade-repo.org/packages/")
)

;; (require 'textexpander-sync)
;; (textexpander-sync)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "/Users/splumlee/.saveplace")

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%R"))
  )

(defun etdo ()
  (interactive)
  (find-file "/Users/splumlee/Dropbox/et/todo.notes")
  )

(defun todo ()
  (interactive)
  (find-file "/Users/splumlee/Dropbox/org/tasks.notes")
  )

(defun reg ()
  (interactive)
  (find-file "/Users/splumlee/git/emacs/regex")
  )

(defun memories ()
  (interactive)
  (find-file "/Users/splumlee/Dropbox/memories.txt")
  (insert "\n")
  (insert (format-time-string "%Y-%m-%d-%R"))
  (insert "\n")
  )

(defun codereview ()
  (interactive)
  (find-file "/Users/splumlee/Dropbox/et/codereviews.txt")
  )

(defun scratch ()
  (interactive)
  (find-file "/Users/splumlee/Dropbox/scratch")
  (insert "\n")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/Users/splumlee/git/solarized-emacs/")
(add-to-list 'custom-theme-load-path "/Users/splumlee/git/solarized-emacs/")
(load-theme 'solarized-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVASCRIPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node and npm installed via homebrew
;; so they use this location for global items
;; (add-to-list 'load-path "/Users/splumlee/git/jshint-mode")
;; (require 'flymake-jshint)

(add-to-list 'load-path "/Users/splumlee/git/js3-mode")
(autoload 'js3-mode "js3" nil t)

(add-to-list 'load-path "/Users/splumlee/git/js-doc")
(require 'js-doc)
(add-hook 'js3-mode-hook
          (lambda ()
              (define-key js3-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js3-mode-map "@" 'js-doc-insert-tag)))

(add-hook 'js3-mode-hook (lambda () (flymake-mode t)))
(setq js3-global-externs 'define)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js3-mode))

(defun js3-mode-tabify ()
	   (save-excursion
		 (goto-char (point-min))
		 (while (re-search-forward "[ \t]+$" nil t)
		   (delete-region (match-beginning 0) (match-end 0)))
		 (goto-char (point-min))
		 (if (search-forward "\t" nil t)
			 (tabify (1- (point)) (point-max))))
	   nil)

(add-hook 'js3-mode-hook
	  '(lambda ()
		 (make-local-variable 'write-contents-hooks)
		 (add-hook 'write-contents-hooks 'js3-mode-tabify)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.b
 ;; If there is more than one, they won't work right.

 ;; js3
	'(js3-auto-indent-p t)			; it's nice for commas to right themselves.
	'(js3-enter-indents-newline t) ; don't need to push tab before typing
	'(js3-indent-on-enter-key t)	; fix indenting before moving on
	'(js3-indent-dots t)
	'(js3-indent-tabs-mode t)
	'(js3-indent-level 4)
	'(js3-expr-indent-offset t)
	'(js3-paren-indent-offset 0)
	'(js3-square-indent-offset 0)
	'(js3-curly-indent-offset 0)
	'(js3-max-columns 80)
	'(js3-mirror-mode t)
	'(js3-mode-escape-quotes nil)
;;	'(js3-global-externs '(define require))
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

