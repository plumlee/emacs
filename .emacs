(setq-default enable-local-variables :safe)

(setq indent-tabs-mode t)
(setq default-tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
:: (set indent-line-function 'insert-tab)

;; (dir-locals-set-class-variables 'git-default
;; 				'((nil . (
;; 					  (indent-tabs-mode . t)
;; 					  (default-tab-width . 4)
;; 					  (tab-stop-list . (number-sequence 4 200 4))
;; 					  ;; (indent-line-function . 'insert-spaces)
;; 					  (sgml-basic-offset . 4)
;; 					  (js3-indent-level . 4)
;; 					  (js3-indent-tabs-mode . t)
;; 					  )))
;; )

;; (dir-locals-set-class-variables 'git-personal
;; 				'((nil . (
;; 					  (indent-tabs-mode . t)
;; 					  (default-tab-width . 4)
;; 					  (tab-stop-list . (number-sequence 4 200 4))
;; 					  ;; (indent-line-function . 'insert-tab)
;; 					  (sgml-basic-offset . 4)
;; 					  (js3-indent-level . 4)
;; 					  (js3-indent-tabs-mode . t)
;; 					  )))
;; )

;; (dir-locals-set-class-variables 'git-plumlee-work
;; 				'((nil . (
;; 					  (indent-tabs-mode . t)
;; 					  (default-tab-width . 4)
;; 					  (tab-stop-list . (number-sequence 4 200 4))
;; 					  (sgml-basic-offset . 4)
;; 					  (js3-indent-level . 4)
;; 					  (js3-indent-tabs-mode . t)
;; 					  )))
;; )

;; (dir-locals-set-class-variables 'git-volo
;; 				'((nil . (
;; 					  (indent-tabs-mode . f)
;; 					  (default-tab-width . 4)
;; 					  (tab-stop-list . (number-sequence 4 200 4))
;; 					  ;; (indent-line-function . 'insert-spaces)
;; 					  (sgml-basic-offset . 4)
;; 					  (js3-indent-level . 4)
;; 					  (js3-indent-tabs-mode . f)
;; 					  (js3-curly-indent-offset . 2)
;; 					  )))
;; )

;; (dir-locals-set-directory-class
;;  "~/git" 'git-default)

;; (dir-locals-set-directory-class
;;  "~/git/personal" 'git-personal)

;; (dir-locals-set-directory-class
;;  "~/git/plumlee-work" 'git-plumlee-work
;; )

;; (dir-locals-set-directory-class
;;  "~/git/third-party/volo" 'git-volo)

(add-to-list 'load-path "/Users/scott/git/emacs/packages")
(add-to-list 'load-path "/Users/scott/git")
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

:: (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

(setq temporary-file-directory "/tmp")

(cua-mode 0)

(global-auto-revert-mode t)

(setq frame-title-format
	  (list (format "%s %%S: %%j " (system-name))
		'(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
	(kill-new (file-truename buffer-file-name))))

;; (defun show-file-name ()
;;	 "Show the full path file name in the minibuffer."
;;	 (interactive)
;;	 (message (buffer-file-name)))

(global-set-key (kbd "<f1>") 'copy-full-path-to-kill-ring)

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
;; ctags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/Users/scott/git/etags-select")
(require 'etags-select);
(load-file "/Users/scott/git/eproject/eproject.el")
(define-project-type stackato (generic)
   (look-for "stackato.yml")
   :relevant-files ("\\.js$"))

(setq path-to-ctags "/Users/scott/git/homebrew/bin/ctags")

;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(defun build-ctags ()
	 (interactive)
	 (message "building project tags")
	 (let ((root (eproject-root)))
	(shell-command (concat "ctags -e -R --extra=+fq --exclude=node_modules --exclude=dist --exclude=.git -f " root "TAGS " root)))
	 (visit-project-tags)
	 (message "tags built successfully"))

(defun visit-project-tags ()
	 (interactive)
	 (let ((tags-file (concat (eproject-root) "TAGS")))
	(visit-tags-table tags-file)
	(message (concat "Loaded " tags-file))))

(defun my-find-tag ()
  (interactive)
  (if (file-exists-p (concat (eproject-root) "TAGS"))
	  (visit-project-tags)
	(build-ctags))
  (etags-select-find-tag-at-point))

(global-set-key (kbd "M-.") 'my-find-tag)



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
(add-to-list 'load-path "/Users/scott/git/markdown-mode")
(require 'markdown-mode);
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; (require 'smartparens);
;; (smartparens-global-mode 1)

(show-paren-mode t)

;; use ; to start abbrevs, so change meaning in syntax table
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)

(modify-syntax-entry ?; "w")

;; os x
(setq mac-command-modifier 'ctrl)
(setq mac-pass-command-to-system nil)
(setq mac-option-modifier 'meta)

;; environment
;; must have paths to jshint and node for emacs-flymake
;; (setenv "PATH" (concat "/usr/local/bin:" "/usr/local/share/npm/lib/node_modules/jshint/bin:" "/usr/local/share/npm/bin:" (getenv "PATH")))
(setenv "NODE_PATH" (concat "/usr/local/bin/node" (concat (getenv "NODE_PATH"))))
(setenv "NODE_NO_READLINE" "1")

;; http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable/8609349#8609349
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
	(setenv "PATH" path-from-shell)
	(setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH);

;; various tools and pieces
(setq make-backup-files nil)

;; (setq backup-file-directory "/Users/scott/backups")
;; (setq backup-directory-alist
;;           `((".*" . ,backup-file-directory)))
;;     (setq auto-save-file-name-transforms
;;           `((".*" ,backup-file-directory t)))

;; (message "Deleting old backup files...")
;; (let ((week (* 60 60 24 7))
;;       (current (float-time (current-time))))
;;   (dolist (file (directory-files backup-file-directory t))
;;     (when (and (backup-file-name-p file)
;;                (> (- current (float-time (fifth (file-attributes file))))
;;                   week))
;;       (message "%s" file)
;;       (delete-file file))))

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

(add-to-list 'load-path "~/git/popup-el/")
(add-to-list 'load-path "~/git/auto-complete/")
(require 'auto-complete-config)
(ac-config-default)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; snippets
(add-to-list 'load-path "/Users/scott/git/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
	  '("~/git/dotfiles/snippets"
		))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
(yas-reload-all)

;; git-gutter
(add-to-list 'load-path "/Users/scott/git/emacs-git-gutter/")
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
(add-to-list 'load-path "/Users/scott/git/emacs-flymake/")
(require 'flymake);
;; (setq flymake-log-level 3)
;; https://github.com/illusori/emacs-flymake
(add-to-list 'load-path "/Users/scott/git/emacs-flymake-cursor/")
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
(setq desktop-path '("/Users/scott/Dropbox/computers/brody"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; abbrevs always on
;; (read-abbrev-file "/Users/scott/.abbrev_defs")
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
;; (setq default-abbrev-mode t)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "/Users/scott/.saveplace")

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

(defun todo ()
  (interactive)
  (find-file "/Users/scott/Dropbox/tasks.notes")
  )

(defun reg ()
  (interactive)
  (find-file "/Users/scott/git/emacs/regex")
  )

(defun memories ()
  (interactive)
  (find-file "/Users/scott/Dropbox/memories.txt")
  (insert "\n")
  (insert (format-time-string "%Y-%m-%d-%R"))
  (insert "\n")
  )

(defun codereview ()
  (interactive)
  (find-file "/Users/scott/Dropbox/et/codereviews.txt")
  )

(defun scratch ()
  (interactive)
  (find-file "/Users/scott/Dropbox/scratch")
  (insert "\n")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/Users/scott/git/solarized-emacs/")
(add-to-list 'custom-theme-load-path "/Users/scott/git/solarized-emacs/")
(load-theme 'solarized-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'web-mode);

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
)

(add-hook 'web-mode-hook  'web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))

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
(add-hook 'js3-mode-hook '(lambda () 
                (local-set-key "\C-x\C-e" 
                                'js-send-last-sexp)
                (local-set-key "\C-\M-x" 
                                'js-send-last-sexp-and-go)
                (local-set-key "\C-cb" 
                                'js-send-buffer)
                (local-set-key "\C-c\C-b" 
                                'js-send-buffer-and-go)
                (local-set-key "\C-cl" 
                                'js-load-file-and-go)
))

;; json mode
(add-to-list 'load-path "/Users/scott/git/json-mode")
(require 'json-mode)
;; (add-hook 'json-mode-hook
;;	  '(lambda ()
;;		 (add-hook 'before-save-hook
;;				   (lambda ()
;;					 (tabify (point-min) (point-max))))
;; ))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . json-mode))

;; node and npm installed via homebrew
;; so they use this location for global items
;; (add-to-list 'load-path "/Users/scott/git/jshint-mode")
;; (require 'flymake-jshint)

(add-to-list 'load-path "/Users/scott/git/js3-mode")
(autoload 'js3-mode "js3" nil t)

(add-to-list 'load-path "/Users/scott/git/js-doc")
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
 '(js3-indent-on-enter-key t)
 '(js3-max-columns 80)
 '(js3-mirror-mode nil)
 '(js3-mode-escape-quotes nil)
 '(js3-mode-global externs)
 '(js3-paren-indent-offset 0)
 '(js3-square-indent-offset 0)
 '(sgml-basic-offset 4)
 '(js3-indent-level 4)
 '(js3-indent-tabs-mode t)

 '(tags-case-fold-search nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
