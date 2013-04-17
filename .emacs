(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

(setq-default indent-tabs-mode t)
(setq default-tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
(setq indent-line-function 'insert-tab)

(setq temporary-file-directory "/tmp")

(cua-mode 0)

;; highlight current line
(global-hl-line-mode +1)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]	 'move-line-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://ergoemacs.org/emacs/emacs_buffer_management.html
(defalias 'list-buffers 'ibuffer)
;; make buffer switch command show suggestions

;; (ido-mode 1)

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
	(while (and (string-match "^*" (buffer-name)) (< i 50))
	  (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
	(while (and (string-match "^*" (buffer-name)) (< i 50))
	  (setq i (1+ i)) (previous-buffer) )))

(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
	(while (and (not (string-match "^*" (buffer-name))) (< i 50))
	  (setq i (1+ i)) (next-buffer) )))

(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
	(while (and (not (string-match "^*" (buffer-name))) (< i 50))
	  (setq i (1+ i)) (previous-buffer) )))

;; sample easy keys

(global-set-key (kbd "<f5>") 'find-file) ; Open file or dir
(global-set-key (kbd "<f6>") 'kill-this-buffer) ; Close file

;; (global-set-key (kbd "<C-prior>") 'previous-user-buffer) ; Ctrl+PageUp
;; (global-set-key (kbd "<C-next>") 'next-user-buffer) ; Ctrl+PageDown
;; (global-set-key (kbd "<C-S-prior>") 'previous-emacs-buffer) ; Ctrl+Shift+PageUp
;; (global-set-key (kbd "<C-S-next>") 'next-emacs-buffer) ; Ctrl+Shift+PageDown

(add-to-list 'load-path "/Users/splumlee/git/emacs/packages")
(require 'markdown-mode);
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))

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
;; auto-complete
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/git/popup-el/")
(require 'auto-complete-config)
(ac-config-default)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; snippets
(add-to-list 'load-path
			  "~/git/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
	  '("~/git/dotfiles/snippets"
		))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
(yas-reload-all)

;; palpimset
;; (add-to-list 'load-path "/Users/splumlee/git/Palimpsest/")
;; (require 'palimpsest);

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
(add-to-list 'load-path "/Users/splumlee/git/emacs-flymake/")
(require 'flymake);
;; (setq flymake-log-level 3)
;; https://github.com/illusori/emacs-flymake
(add-to-list 'load-path "/Users/splumlee/git/emacs-flymake-cursor/")
(add-hook 'find-file-hook 'flymake-find-file-hook)
;; https://github.com/illusori/emacs-flymake-cursor
(eval-after-load 'flymake '(require 'flymake-cursor))
;; (eval-after-load 'flymake 
;;	 '(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;;	 (setq flymake-check-was-interrupted t))
;;	 (ad-activate 'flymake-post-syntax-check))
(setq flymake-max-parallel-syntax-checks 8)

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

(require 'textexpander-sync)
(textexpander-sync)
(setq default-abbrev-mode t)

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
(defun intm ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%R"))
  )

(defun indt ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d"))
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
;; HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.hbs$" . html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVASCRIPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js REPL using NODE
(require 'js-comint)
;;	Set inferior-js-program-command to the execution command for running your javascript REPL
;; Use node as our repl
(setq inferior-js-program-command "node")
 
(setq inferior-js-mode-hook
	  (lambda ()
		;; We like nice colors
		(ansi-color-for-comint-mode-on)
		;; Deal with some prompt nonsense
		(add-to-list 'comint-preoutput-filter-functions
					 (lambda (output)
					   (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
					   (replace-regexp-in-string ".*1G.*3G" "&gt;" output)
					   (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)
)))))


;; json mode
(add-to-list 'load-path "/Users/splumlee/git/json-mode")
(require 'json-mode)
(add-hook 'json-mode-hook
	  '(lambda ()
		 (add-hook 'before-save-hook
				   (lambda ()
					 (tabify (point-min) (point-max))))
))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . json-mode))

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
(add-hook 'js3-mode-hook (lambda () (yas-minor-mode)))
(add-hook 'js3-mode-hook (lambda () (auto-complete-mode t)))
(setq js3-global-externs 'define)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-hook 'js3-mode-hook
		  '(lambda ()
			   (add-hook 'before-save-hook
						 (lambda ()
							 (tabify (point-min) (point-max))))
			   ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-run-in-place nil)
 '(js3-auto-indent-p t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-curly-indent-offset 0)
 '(js3-enter-indents-newline t)
 '(js3-expr-indent-offset 0)
 '(js3-global-externs (mapcar (quote symbol-name) (quote (require define JSON process __dirname console exports))))
 '(js3-indent-dots t)
 '(js3-indent-level 4)
 '(js3-indent-on-enter-key t)
 '(js3-indent-tabs-mode t)
 '(js3-max-columns 80)
 '(js3-mirror-mode t)
 '(js3-mode-escape-quotes nil)
 '(js3-mode-global externs)
 '(js3-paren-indent-offset 0)
 '(js3-square-indent-offset 0)
 '(sgml-basic-offset 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
