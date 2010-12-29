(add-to-list 'load-path "~/repo/emacs/packages/")
(add-to-list 'load-path "~/repo/emacs/packages/color-theme-6.6.0")

(setq mac-command-modifier 'meta)

;; common lisp
(require 'cl)

;; keep backup files in one place
(setq backup-directory-alist (quote ((".*" . "~/backups/"))))
(setq auto-save-file-name-transforms `((".*" ,"~/backups/" t)))

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

;; tabs and spaces
;; always use spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; tab key goes over 4
(setq c-basic-offset 4)
;; interpret tab to be 4 characters wide, and tab stops 4 wide
(setq-default tab-width 4)
(setq-default standard-indent 4)

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
  (find-file "~/git/emacs/regex")
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
