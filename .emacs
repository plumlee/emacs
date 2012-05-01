;; use ; to start abbrevs, so change meaning in syntax table
(modify-syntax-entry ?; "w")

;; keep what's on disk in the buffers
;; should prevent bad habits too
(global-auto-revert-mode t)

;; spreading the marmalade
(if (not (getenv "TERM_PROGRAM"))
       (setenv "PATH"
               (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))
(add-to-list 'exec-path "~/usr/local/bin")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; change frames
(global-set-key "\M-`" 'other-frame)

;; ANNOYING
(setq visible-bell t)

;; desktop mode
(desktop-save-mode 1)
(setq desktop-path '("~/Dropbox/emacs"))

(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; REGEX
(require 're-builder)
(setq reb-re-syntax 'string)
(defun reb-query-replace (to-string)
      "Replace current RE from point with `query-replace-regexp'."
      (interactive
       (progn (barf-if-buffer-read-only)
              (list (query-replace-read-to (reb-target-binding reb-regexp)
                                           "Query replace"  t))))
      (with-current-buffer reb-target-buffer
        (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

;; os x
(setq mac-command-modifier 'ctrl)
(setq mac-pass-command-to-system nil)
(setq mac-option-modifier 'meta)

(cua-mode 0)
(auto-fill-mode -1)
;;(setq fill-column 100)
(show-paren-mode 1)

;; Don't tabify after rectangle commands
;; (setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
;; No region when it is not highlighted
(setq cua-keep-region-after-copy t)


(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)

(add-to-list 'load-path "~/git/emacs/packages/")

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
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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

;; abbrevs always on
(read-abbrev-file "~/.abbrev_defs")
(setq abbrev-mode t)
(setq save-abbrevs t)

(require 'textexpander-sync)
;;(textexpander-sync)

;; steve yegge
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
		(filename (buffer-file-name)))
	(if (not filename)
		(message "Buffer '%s' is not visiting a file!" name)
	  (if (get-buffer new-name)
		  (message "A buffer named '%s' already exists!" new-name)
		(progn	 (rename-file name new-name 1)	 (rename-buffer new-name)	 (set-visited-file-name new-name)	 (set-buffer-modified-p nil))))))

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
	  (progn	(copy-file filename newname 1)	(delete-file filename)	(set-visited-file-name newname)		(set-buffer-modified-p nil)		t))))

;; tabs and spaces
;; use tabs
(setq indent-tabs-mode t)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-basic-offset 4)
(setq tab-width 4)
(setq standard-indent 4)
(setq sgml-basic-offset 4)
(defun tabify-buffer ()
  "Tabify current buffer"
  (interactive)
  (tabify (point-min) (point-max)))

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

;; buffer flipping
;; http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun transpose-windows ()
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond ((not (= (count-windows) 2))
        (message "You need exactly 2 windows to do this."))
       (t
        (let* ((w1 (first (window-list)))
               (w2 (second (window-list)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (other-window 1)))))
(global-set-key (kbd "<f7>") 'transpose-buffers)

;; clear up files before saving them
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file and leaves single newline character."
  (interactive)
  (save-excursion
	(goto-char (point-max))
	(newline)			   ;; ensures that there is at least one
	(delete-blank-lines))) ;; leaves at most one

(defun indent-whole-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil)
  (tabify (point-min) (point-max)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-blank-lines)
;;(add-hook 'before-save-hook 'tabify)

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
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; dos mode
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;; line numbers
(add-hook 'org-mode-hook
		  (lambda ()
			(linum-mode t))
		  t)
;;(global-line-number-mode t)

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

;;(split-window-horizontally)

(setq save-place-file "~/.emacs.d/saveplace")
(setq save-place t)
(require 'saveplace)

;; aspx files
;; (autoload 'aspx-mode "aspx-mode.el" "Edit ASPX files." t)
;; (setq auto-mode-alist (append '(("\\.aspx$" . aspx-mode))
;;					   auto-mode-alist))

;; file names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; auto-byte-compile
;; (add-hook 'emacs-lisp-mode-hook '(lambda ()
;; 								   (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))
;; 		  )

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq special-display-buffer-names
           '("shell"))
(shell)
;; (add-hook 'after-init-hook #'(lambda ()
;;								 (split-window-vertically)
;;								 (other-window 1)
;;								 (run-scheme "you-are-here")))
(add-hook 'comint-mode-hook
		  (lambda ()
			(define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
			(define-key comint-mode-map (kbd "C-n") 'comint-next-input)
			))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVASCRIPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/git/emacs/packages/js3-mode")
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . abbrev-mode))
(add-hook 'js3-mode-hook '(lambda () (auto-fill-mode nil)))
(add-hook 'js3-mode-hook '(lambda () (auto-fill-mode nil)))

(defalias 'jm 'js3-mode)
(defalias 'jd 'insert-javascript-doc)
(defalias 'jt 'insert-javascript-todo)
(defalias 'jsdelim 'insert-js-section-delimiter)

;; put in js comment structure
(defun insert-javascript-doc ()
  (interactive)
  (insert "\n/**")
  (insert "\n * ")
  (insert "\n * @param {}")
  (insert "\n */")
  (previous-line 1)
  (end-of-line)
  (backward-char )
  )

;; put in js comment structure
(defun insert-javascript-todo ()
  (interactive)
  (insert "\n/**")
  (insert "\n * TODO ")
  (insert "\n */")
  (previous-line 1)
  (end-of-line)
  )

;; put in comment blocks
(defun insert-js-section-delimiter ()
  (interactive)
  (insert "//////////////////////////////////////////////////")
  (insert "\n// ")
  (insert "\n//////////////////////////////////////////////////")
  (insert "\n")
  (previous-line 2)
  (end-of-line)
  )


(setq auto-mode-alist (append '(("\\.json$" . js3-mode))
							  auto-mode-alist))
(setq auto-mode-alist (append '(("\\.js$" . js3-mode))
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
(add-hook 'js3-mode-hook
		  (lambda ()
			;; Scan the file for nested code blocks
			(imenu-add-menubar-index)
			;; Activate the folding mode
			(hs-minor-mode t)))

;; add predefined values to jslint
(setq js3-global-externs (split-string "Buffer, clearInterval, clearTimeout, console, exports, global, module, process, querystring, require, setInterval, setTimeout, __filename, __dirname" "[ ,]+" t))

;; http://www.emacswiki.org/emacs/Js2Mode
;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
    ;; add any symbols to a buffer-local var of acceptable global vars
    ;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
    ;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
    ;; you can;t have a symbol called "someName:false"
(add-hook 'js3-post-parse-callbacks
          (lambda ()
            (let ((btext (replace-regexp-in-string
                          ": *true" " "
                          (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
              (setq js3-additional-externs
                    (split-string
                     (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                     " *, *" t))
              )))

;; TODO
;; syntax for doing this in highlighted region
;; (defun flip-js-function-declaration ()
;;   (string-match "^\(?:[ ]+\)?\(function\) \([^( ]+\) ?(\(.*\)) ?{"
;;   (replace-regexp-in-string  "\2: \1 (\3) {")
;; )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; END JAVASCRIPT MODE
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/git/emacs/packages/magit")
(require 'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'flymake-csslint)
;; (add-hook 'css-mode-hook 'flymake-mode)
;; (setq flymake-gui-warnings-enabled nil)
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
