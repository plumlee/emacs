(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

(setq-default indent-tabs-mode t)
(setq default-tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
(setq indent-line-function 'insert-tab)

(cua-mode 0)

(show-paren-mode t)

;; use ; to start abbrevs, so change meaning in syntax table
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)

(global-auto-revert-mode t)

(modify-syntax-entry ?; "w")

(add-to-list 'load-path "~/git/emacs/packages/")

;; os x
(setq mac-command-modifier 'ctrl)
(setq mac-pass-command-to-system nil)
(setq mac-option-modifier 'meta)

;; environment
(setenv "PATH" (getenv "PATH"))
(setenv "NODE_PATH" (getenv "NODE_PATH"))
(setq exec-path
	  '(
	"~/git/homebrew/bin"
	"~/git/homebrew/share/npm/lib/node_modules"
	"~/git/homebrew/share/npm/bin/node_modules"
	"/usr/bin"
	"/bin"
	))

;; various tools and pieces
;; keep backup files in one place
(setq backup-directory-alist (quote ((".*" . "~/backups/" ))))
(setq auto-save-file-name-transforms `((".*", "~/backups/" t)))

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
;; flymake mode
;; https://github.com/illusori/emacs-flymake
;; (add-to-list 'load-path "~/git/emacs-flymake")
(require 'flymake);
;; https://github.com/illusori/emacs-flymake-cursor
;; (add-to-list 'load-path "~/git/emacs-flymake-cursor")
(add-hook 'find-file-hook 'flymake-find-file-hook)
(eval-after-load 'flymake '(require 'flymake-cursor))
;; (setq flymake-max-parallel-syntax-checks 8)
;; (setq flymake-run-in-place nil)
;; (setq temporary-file-directory "/tmp")
;; (setq flymake-number-of-errors-to-display 4)

;; desktop mode
(desktop-save-mode 1)
(setq desktop-path '("~/Dropbox"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; abbrevs always on
;; (read-abbrev-file "~/.abbrev_defs")
;; (setq abbrev-mode t)
;; (setq save-abbrevs t)

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
(setq save-place-file "~/.saveplace")

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

(defun codereview ()
  (interactive)
  (find-file "~/Dropbox/et/codereviews.txt")
  )

(defun scrach ()
  (interactive)
  (find-file "~/Dropbox/scrach")
  (insert "\n")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/git/solarized-emacs/")
(add-to-list 'custom-theme-load-path "~/git/solarized-emacs/")
(load-theme 'solarized-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVASCRIPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node and npm installed via homebrew
;; so they use this location for global items
(add-to-list 'load-path "~/git/jshint-mode")
(require 'flymake-jshint)
(add-hook 'js3-mode-hook
		  (lambda () (flymake-mode t))
)

(add-to-list 'load-path "~/git/js3-mode")
(autoload 'js3-mode "js3" nil t)
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
	;; let jshint do this instead, js3 can't parse /*global foo:true */
	'(js3-highlight-external-variables nil)

;; jshint
	'(jshint-mode-jshintrc "~/.jshintrc")
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
