;; (setq line-spacing 0.5)

(setq auto-save-default nil)

(setq inhibit-splash-screen t
      inhibit-startup-message t)

(setq visible-bell 'top-bottom)
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(require 'package)
;;(require 'melpa)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(setq url-http-attempt-keepalives nil)

(defvar prelude-packages
  '(
    ac-helm
    anzu
    async
    auto-complete
    autopair
    avy
    buffer-move
    coffee-mode
    dash
    dash-at-point
    dash-functional
    epl
    expand-region
    f
    flx
    flx-ido
    flycheck
    fringe-helper
    git-commit
    git-gutter
    git-gutter-fringe
    git-timemachine
    gitconfig-mode
    gradle-mode
    handlebars-mode
    helm
    helm-core
    js-comint
    js-doc
    js2-mode
    js3-mode
    json-mode
    json-reformat
    json-snatcher
    less-css-mode
    let-alist
    literate-coffee-mode
    magit
    magit-popup
   markdown-mode
    mediawiki
    nvm
    pkg-info
    popup
    projectile
    s
    seq
    shrink-whitespace
    smartparens
    solarized-theme
    undo-tree
    visual-regexp
    visual-regexp-steroids
    web-mode
    whitespace-cleanup-mode
    with-editor
   yasnippet
    )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (every #'package-installed-p prelude-packages))

(defun prelude-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package prelude-packages)
    (add-to-list 'prelude-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'prelude-require-package packages))

(define-obsolete-function-alias 'prelude-ensure-module-deps 'prelude-require-packages)

(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (prelude-require-packages prelude-packages)))

;; run package installation
(prelude-install-packages)

(setq ns-use-srgb-colorspace t)

(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f2>") 'cut-line-or-region) ; cut.
(global-set-key (kbd "<f3>") 'copy-line-or-region) ; copy.
(global-set-key (kbd "<f4>") 'yank) ; paste.
(global-set-key (kbd "<f5>") 'comment-or-uncomment-region-or-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq HOME (expand-file-name "~/"))
(setq PATH (getenv "PATH"))
(setq create-lockfiles nil)
(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/backups")))
(setenv "NODE_NO_READLINE" "1")
(setq temporary-file-directory "/tmp")
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LINE SPACING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq line-spacing 0.5) ; add 0.5 height between lines
    (setq line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-frame (selected-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUT/COPY/PASTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )

(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHITESPACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'whitespace)
(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq whitespace-line-column 76)
(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab
                                  indentation empty
                                  space-after-tab))

(setq whitespace-style '(face empty tabs lines-tail trailing))
;; delete blank lines and whitespace
(global-set-key (kbd "M-SPC") 'shrink-whitespace)
;; (setq whitespace-global-modes '(js3-mode coffee-mode web-mode markdown-mode))
;; (global-whitespace-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(visual-line-mode)
(column-number-mode)
(global-linum-mode 1)

(setq default-buffer-file-coding-system 'utf-8-unix)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq require-final-newline t)
(setq sgml-basic-offset 4)
(setq python-indent-offset 4)
(setq coding-system-for-write 'utf-8-unix)

(global-set-key (kbd "C--") 'undo-tree-undo)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "M--") 'undo-tree-redo)
(global-set-key (kbd "M-y") 'undo-tree-redo)

(global-hl-line-mode +1)

;; another way for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; ;; Backwords kill
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; die scrollbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALIASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;(defalias 'fe 'flymake-display-err-menu-for-current-line)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'wc 'whitespace-cleanup)
(defalias 'arx 'align-regexp)

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
  (find-file "/Users/scott/Dropbox/todo.txt")
  )

(defun reg ()
  (interactive)
  (find-file "/Users/scott/git/emacs/regex")
  )

(defun codereview ()
  (interactive)
  (find-file "/Users/scott/Dropbox/et/codereviews.txt")
  )

(defun scratch ()
  (interactive)
  (find-file "/Users/e008163/Dropbox/scratch")
  (insert "\n")
  )

;; http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'load-path "/Users/scott/git/solarized-emacs/")
;;(add-to-list 'custom-theme-load-path "/Users/scott/git/solarized-emacs/")
(load-theme 'solarized-light t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YA SNIPPET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/git/dotfiles/snippets"
        ))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
(yas-reload-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RECENT MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COFFEE MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/bodil/emacs.d/blob/master/bodil-js.el

;; (require 'coffee-mode)

;; (add-hook 'coffee-mode-hook
;;           (lambda ()
;;             (whitespace-mode t)
;;             (define-key coffee-mode-map (kbd "M-R") 'coffee-compile-buffer)
;;             (define-key coffee-mode-map (kbd "M-r") 'coffee-compile-region)
;;             (define-key coffee-mode-map (kbd "<tab>") 'coffee-indent)
;;             (define-key coffee-mode-map (kbd "<backtab>") 'coffee-unindent))
;;           )


;; (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;; (add-to-list 'auto-mode-alist '("\\.cson$" . coffee-mode))
;; (setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap
;; (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

;; ;; Use js2-mode for displaying compiled CS
;; (setq coffee-js-mode 'js3-mode)

;; ;; Patch coffee-mode so coffee-compile-region pops up a new
;; ;; non-focused window instead of replacing the current buffer.
;; (eval-after-load "coffee-mode"
;;   '(defun coffee-compile-region (start end)
;;      "Compiles a region and displays the JS in another buffer."
;;      (interactive "r")
;;      (let ((buffer (get-buffer coffee-compiled-buffer-name)))
;;        (when buffer (kill-buffer buffer)))
;;      (call-process-region start end coffee-command nil
;;                           (get-buffer-create coffee-compiled-buffer-name) nil "-s" "-p" "--bare")
;;      (let ((buffer (get-buffer coffee-compiled-buffer-name)))
;;        (with-current-buffer buffer
;;          (funcall coffee-js-mode)
;;          (goto-char (point-min)))
;;        (display-buffer buffer))))

;; ;; Handle backtabs and indenting regions
;; (defun coffee-indent-block ()
;;   (shift-region coffee-tab-width)
;;   (setq deactivate-mark nil))

;; (defun coffee-unindent-block ()
;;   (shift-region (- coffee-tab-width))
;;   (setq deactivate-mark nl))

;; (defun coffee-indent ()
;;   (interactive)
;;   (if (and (boundp 'ac-trigger-command-p) (ac-trigger-command-p last-command))
;;       (auto-complete)
;;     (if mark-active
;;         (coffee-indent-block)
;;       (indent-for-tab-command))))

;; (defun coffee-unindent ()
;;   (interactive)
;;   (if mark-active
;;       (coffee-unindent-block)
;;     (progn
;;       (indent-line-to (- (current-indentation) coffee-tab-width)))))

;; (setq coffee-tab-width 4)
;; (eval-after-load 'coffee-mode
;;   '(define-key coffee-mode-map (kbd "M-r") 'coffee-compile-buffer))
;; (eval-after-load 'coffee-mode
;;   '(define-key coffee-mode-map (kbd "C-c f") 'coffee-compile-file))

;; ;; (setq flymake-coffee-coffeelint-configuration-file
;; ;;       (concat HOME ".coffeelintrc"))
;; ;; (require 'flymake-coffee)
;; ;; (add-hook 'coffee-mode-hook 'flymake-coffee-load)
;; (add-hook 'coffee-mode-hook 'auto-complete-mode)
;; (add-hook 'coffee-mode-hook 'whitespace-mode)
;; (add-to-list 'auto-mode-alist '("\\.coffeelintrc$" . json-mode))
;; (add-hook 'coffee-mode-hook 'linum-mode)
;; (setq whitespace-action '(auto-cleanup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LITERATE COFFEE MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/syohex/emacs-literate-coffee-mode
;; (require 'literate-coffee-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-clock-persist 'history)
     (org-clock-persistence-insinuate)
(add-hook 'org-mode-hook 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT-GUTTER-FRINGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'git-gutter-fringe)
(global-git-gutter-mode t)
;; bind git-gutter toggle command
(global-set-key (kbd "C-x C-g") 'git-gutter-fr:toggle)
;; Jump to next/previous diff
(global-set-key (kbd "C-x p") 'git-gutter-fr:previous-diff)
(global-set-key (kbd "C-x n") 'git-gutter-fr:next-diff)
;; ignore all spaces
(setq git-gutter-fr:diff-option "-w")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS-COMINT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'js-comint)
;;  Set inferior-js-program-command to the execution command for running your javascript REPL
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
;; (add-hook 'js3-mode-hook '(lambda ()
;;                 (local-set-key "\C-x\C-e"
;;                                 'js-send-last-sexp)
;;                 (local-set-key "\C-\M-x"
;;                                 'js-send-last-sexp-and-go)
;;                 (local-set-key "\C-cb"
;;                                 'js-send-buffer)
;;                 (local-set-key "\C-c\C-b"
;;                                 'js-send-buffer-and-go)
;;                 (local-set-key "\C-cl"
;;                                 'js-load-file-and-go)
;; ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS3-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'js3-mode)
(add-hook 'js3-mode-hook
      (lambda ()
        (hs-minor-mode t)
        (whitespace-mode t)
        ;; (setq js3-auto-indent-p t)
        (setq js3-consistent-level-indent-inner-bracket t)
        ;; (setq js3-curly-indent-offset 0)
        ;; (setq js3-enter-indents-newline t)
        ;; (setq js3-expr-indent-offset 0)
        ;; (setq js3-indent-dots t)
        ;; (setq js3-lazy-dots t)
        ;; (setq js3-indent-on-enter-key t)
        ;; (setq js3-max-columns 80)
        ;; (setq js3-mirror-mode nil)
        ;; (setq js3-mode-escape-quotes nil)
        ;; (setq js3-paren-indent-offset 0)
        ;; (setq js3-square-indent-offset 0)
        (setq sgml-basic-offset 4)
        (setq js3-indent-level 4)
        (setq js3-indent-tabs-mode nil)
        (setq js3-cleanup-whitespace t)
        ;; (setq js3-mode-escape-quotes nil)
        (setq js3-global-externs (mapcar (quote symbol-name) (quote (require define JSON module process __dirname console exports global $ descibe beforeEach afterEach it expect))))
        ;; (setq js3-mode-show-parse-errors nil)
        ;; (setq js3-mode-show-strict-warnings nil)
        ;; (setq js3-strict-cond-assign-warning nil)
        ;; (setq js3-strict-inconsistent-return-warning nil)
        ;; (setq js3-strict-missing-semi-warning nil)
        ;; (setq js3-strict-trailing-comma-warning nil)
        ;; (setq js3-strict-var-hides-function-arg-warning nil)
        ;; (setq js3-strict-var-redeclaration-warning nil)
        ;; (yas-minor-mode)
        (auto-complete-mode t)
        ;; (local-set-key "\C-x\C-e" 'js-send-last-sexp)
        ;; (local-set-key "\C-\M-x" js-send-last-sexp-and-go)
        ;; (local-set-key "\C-cb" js-send-buffer)
        ;; (local-set-key "\C-c\C-b" js-send-buffer-and-go)
        ;; (local-set-key "\C-cl" js-load-file-and-go)
        )
      )

;; (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS2-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-mode-hook
  '(lambda () (progn
    (set-variable 'indent-tabs-mode nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'json-mode-hook
      (lambda ()
        (whitespace-mode t)
        )
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)
;; (add-to-list 'dash-at-point-mode-alist '(js3-mode . "backbone"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'web-mode)

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-script-padding 4)
  (setq sgml-basic-offset 4)
  (whitespace-mode t)
  )

(add-hook 'web-mode-hook  'web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.less?\\'" . less-css-mode))
(add-hook 'css-mode-hook
      (lambda ()
        (whitespace-mode t)
        )
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'auto-mode-alist '("\\.txt$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-hook 'gfm-mode-hook
          (lambda ()
            (smartparens-mode nil)
            (whitespace-mode t)
            (setq whitespace-line-column 1000)
          )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTOCOMPLETE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARENS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smartparens);
(smartparens-global-mode 1)
(show-paren-mode t)
(add-hook 'smartparens-mode-hook
      (lambda ()
        (setq sp-autoescape-string-quote nil)
        )
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER MOVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTAGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq path-to-ctags "/opt/local/bin/ctags")
(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )
(defadvice find-tag (around refresh-etags activate)
   "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
    ad-do-it
      (error (and (buffer-modified-p)
          (not (ding))
          (y-or-n-p "Buffer is modified, save it? ")
          (save-buffer))
         (er-refresh-etags extension)
         ad-do-it))))
  (defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO/IMENU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'imenu)
(require 'ido)
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(global-set-key (kbd "C-t")  'ido-goto-symbol)
;; make ido display choices vertically
(setq ido-separator "\n")

;; display any item that contains the chars you typed
(setq ido-enable-flex-matching t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ABBREV MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq default-abbrev-mode t)
;;(require 'textexpander-sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDO-TREE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUA-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode 0)
;; (global-set-key [(alt c)] 'kill-ring-save)
;; (global-set-key [(alt v)] 'yank)
;; (global-set-key [(alt x)] 'kill-region)
;; (global-set-key [(alt s)] 'save-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECTILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (projectile-global-mode)
;; (setq projectile-enable-caching t)
;; https://github.com/lewang/flx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq gc-cons-threshold 20000000)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".js" ".txt"))
(setq ido-ignore-extensions t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ac-helm) ;; Not necessary if using ELPA package
(global-set-key (kbd "C-c h") 'helm-mini)
;; (helm-mode 1)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AVY 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)
(avy-setup-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPAND REGION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dir-locals-set-class-variables '~/git/browserify-istanbul
				'((nil . (
					  (indent-tabs-mode . t)
					  (default-tab-width . 4)
					  (tab-stop-list . (number-sequence 4 200 4))
					  ;; (indent-line-function . 'insert-spaces)
					  (sgml-basic-offset . 4)
					  (js3-indent-level . 4)
					  (js3-indent-tabs-mode . t)
					  )))
)


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it. 
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

(define-key emacs-lisp-mode-map (kbd "C-M-;")
  #'comment-or-uncomment-sexp)
(eval-after-load 'js3-mode
  '(define-key js3-mode-map (kbd "C-M-;")
     #'comment-or-uncomment-sexp))


(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dash-at-point yasnippet whitespace-cleanup-mode web-mode visual-regexp-steroids undo-tree solarized-theme smartparens shrink-whitespace projectile mediawiki markdown-mode magit literate-coffee-mode less-css-mode json-mode js3-mode js2-mode js-doc js-comint handlebars-mode groovy-mode gradle-mode gitconfig-mode git-timemachine git-gutter-fringe flycheck flx-ido expand-region buffer-move avy autopair anzu ac-helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
