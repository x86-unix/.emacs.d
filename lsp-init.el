(setq tramp-connection-timeout 5)

;; bar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Display full path of file in title bar
(setq frame-title-format "%f")

;; no messages
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Window size
(setq initial-frame-alist
      (append
       (list
        '(width . 180) ; window width
        '(height . 50) ; window height
        '(top . 0) ; window position from the top
        )
       initial-frame-alist))
(setq default-frame-alsit initial-frame-alist)

;; Depth/length of evaluation result output
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; add melpa repos
(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(package-initialize)

;; initial install packages
(defvar my-install-package-list
  '(use-package
    ;; Add more packages here...
    )
  "Install Initial Packages")

(dolist (pkg my-install-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Update all installed packages
(defun update-all-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (package (mapcar 'car package-alist))
    (let ((latest-version
           (cadr (assq package package-archive-contents))))
      (when (and latest-version
                 (version-list-<
                  (package-desc-version
                   (cadr (assq package package-alist)))
                  (package-desc-version latest-version)))
        (unless (equal package 'gnu-elpa-keyring-update)
          (package-install package)
          (let ((old-package (cadr (assq package package-alist))))
            (when old-package
              (package-delete old-package))))))))
(global-set-key (kbd "C-c u") 'update-all-packages)

;; Load-Path
; Auto add load-path recursively
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
             (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Specify the load path under elpa
(add-to-load-path "elpa")

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package 
(require 'use-package)
(setq use-package-always-ensure t)

;; UTF-8
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; font
(when (eq system-type 'gnu/linux) ; for Linux
  (set-frame-font "Hack")
  (add-to-list 'default-frame-alist '(font . "Hack-12")))
(when (eq system-type 'darwin) ; for Mac
  (set-frame-font "Osaka")
  (add-to-list 'default-frame-alist '(font . "Osaka-12")))
(when (eq system-type 'windows-nt) ; for Windows
  (set-frame-font "BIZ UDゴシック")
  (add-to-list 'default-frame-alist '(font . "BIZ UDゴシック-12")))

;; mode line
(display-time-mode t)

;; reloading the file
(global-set-key
 (kbd "C-c f")
 (lambda ()
   (interactive)
   (revert-buffer :ignore-auto :noconfirm)
   (message "file reloaded.")))

;; reloading the buffer
(global-set-key (kbd "C-c b") 'revert-buffer)

;; assign C-h to Backspace
; Backspace even in mini buffer
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; on mouse yank
(setq mouse-drag-copy-region t)

;; disable beep sound
(setq visible-bell t)

;; Highlight tabs, double-byte spaces, etc.
(global-whitespace-mode t)

(setq whitespace-style
      '(face tabs tab-mark spaces space-mark newline newline-mark))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?□]) ; Full-width space shape
        (space-mark ?\u0020 [?\xB7]) ; Half-width space shape
        (newline-mark ?\n [?↓?\n]) ; carriage return shape
        (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t]) ; TAB shape
        ))

;; Toggle behavior of word wrap
(setq-default truncate-lines nil)
(defun toggle-truncate-lines ()
  "Toggle truncat lines"
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))
(global-set-key (kbd "C-c l") 'toggle-truncate-lines) ; ON/OFF

;; ido-mode
(require 'ido)
(ido-mode t)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
; ido-vertical-mode
(use-package
 ido-vertical-mode
 :init (ido-mode 1) (ido-vertical-mode 1))

;; ace-window
(use-package ace-window :bind ("C-x o" . ace-window))

;; Resize Window
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx
         (if (= (nth 0 (window-edges)) 0)
             1
           -1))
        (dy
         (if (= (nth 1 (window-edges)) 0)
             1
           -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]" (window-width) (window-height))
        (setq c (read-char))
        (cond
         ((= c ?l)
          (enlarge-window-horizontally dx))
         ((= c ?h)
          (shrink-window-horizontally dx))
         ((= c ?j)
          (enlarge-window dy))
         ((= c ?k)
          (shrink-window dy))
         ;; otherwise
         (t
          (message "Quit")
          (throw 'end-flag t)))))))
(global-set-key (kbd "C-c w") 'window-resizer)

;; Tab
(setq-default
 tab-width 4
 indent-tabs-mode nil)

;; Automatic backup settings
; Collect backup files and autosave files into ~/.emacs.d/backups/
(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; Setting the frame title
(setq frame-title-format "%b")
;; Perform backup version control
(setq version-control t)
;; how many new ones to leave
(setq kept-new-versions 10)
;; how many old ones to leave
(setq kept-old-versions 10)
;; Do not ask for confirmation to erase old versions.
(setq delete-old-versions t)

;; Show line numbers
;; Use `display-line-numbers-mode` if Emacs version is 26.1 or later
(if (version<= "26.1" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      ;; 0: normal line number display
      ;; t: do not display line numbers
      (setq display-line-numbers 'relative))
  ;; Use `global-linum-mode` for versions earlier than 26.1
  (global-linum-mode))

;; Theme
(use-package gruvbox-theme :config (load-theme 'gruvbox-dark-hard t))
(set-face-foreground 'font-lock-comment-face "purple")

;; rainbow-delimiters
(use-package
 rainbow-delimiters
 :hook (prog-mode . rainbow-delimiters-mode))

;; Blink corresponding paren
(show-paren-mode 1)

;; visualize indent
(use-package
 highlight-indent-guides
 :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
 :custom (highlight-indent-guides-method 'column))

;; dired
; recentf-ext
(use-package
 recentf-ext
 :config
 (setq recentf-max-saved-items 100) ; Save up to 100 as history
 :bind ("C-c n" . recentf-open-files))

;; open-junk-file
(use-package
 open-junk-file
 :config
 (setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S")
 :bind ("C-c j" . open-junk-file))

;; all-the-icons
; need to manually install fonts with M-x all-the-icons-install-fonts
(use-package all-the-icons :if (display-graphic-p))

;; dired-sidebar
(use-package
 dired-sidebar
 :commands (dired-sidebar-toggle-sidebar)
 :bind (("C-c t" . dired-sidebar-toggle-sidebar))
 :config
 (setq dired-sidebar-show-hidden-files t)
 (setq dired-sidebar-width 30))

(use-package
 all-the-icons-dired
 :hook (dired-mode . all-the-icons-dired-mode))

;; company
(use-package
 company
 :init
 :config (setq company-dabbrev-downcase nil)
 ; without delay.
 (setq company-idle-delay 0)
 ; Default is 4. Make completion start from fewer characters.
 (setq company-minimum-prefix-length 2)
 ; If you try to go further down at the bottom of the candidate, it will return to the top.
 (setq company-selection-wrap-around t)
 ; Show number.
 (setq company-show-numbers t) (global-company-mode)
 :bind
 (:map
  company-active-map
  ("C-n" . company-select-next)
  ("C-p" . company-select-previous)
  ("C-s" . company-filter-candidates)
  ("<tab>" . company-complete-selection))
 :bind
 (:map
  company-search-map
  ("C-n" . company-select-next)
  ("C-p" . company-select-previous)))

;; company-quickhelp
(use-package
 company-quickhelp
 :after company
 :config (company-quickhelp-mode t))

;; disable-company-mode lists
(defvar my-disable-company-modes '(shell-mode eshell-mode term-mode))
(defun my-disable-company-in-selected-modes ()
  (when (apply 'derived-mode-p my-disable-company-modes)
    (company-mode -1)))
(mapc
 (lambda (mode)
   (add-hook
    (intern (concat (symbol-name mode) "-hook"))
    'my-disable-company-in-selected-modes))
 my-disable-company-modes)

;; company-shell
(use-package
 company-shell
 :config (add-to-list 'company-backends 'company-shell))

;; smartparens 
(use-package
 smartparens
 :config (require 'smartparens-config) (smartparens-global-mode t))

;; electric-pair
(use-package
 electric
 :init
 (progn
   (electric-pair-mode 1)))

;; shell-pop
(use-package
 shell-pop
 :init
 (unless (eq system-type 'windows-nt)
   (setq shell-pop-shell-type
         (cond
          ((eq system-type 'gnu/linux)
           '("ansi-term"
             "*ansi-term*"
             (lambda () (ansi-term "/bin/bash"))))
          ((eq system-type 'darwin)
           '("ansi-term"
             "*ansi-term*"
             (lambda () (ansi-term "/bin/zsh"))))))
   (setq shell-pop-full-span t)
   (setq shell-pop-window-size 20)) ; Specify window size as 20% of screen height
 :bind (("C-c s" . shell-pop))
 :config
 (defun my/shell-mode-setup ()
   (define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)
   (define-key
    shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)
   (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
   (define-key shell-mode-map (kbd "C-n") 'comint-next-input))
 (add-hook 'shell-mode-hook 'my/shell-mode-setup))

;; elisp formatter
(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . my/elisp-autofmt-setup))

(defun my/elisp-autofmt-setup ()
  (setq-local before-save-hook
              (lambda ()
                (elisp-autofmt-buffer)
                (setq tab-width 2)
                (setq indent-tabs-mode nil))))
;; black
(use-package
 blacken
 :config (add-hook 'python-mode-hook 'blacken-mode))

;; regular expression support
(use-package regex-tool :bind (("C-c r" . regex-tool)))

;; lsp-mode
(use-package lsp-mode :commands lsp :hook (python-mode . lsp))

;; for python
; npm install -g pyright
(setq python-indent-guess-indent-offset-verbose nil)
(use-package
 lsp-pyright
 :hook
 (python-mode
  .
  (lambda ()
    (require 'lsp-pyright)
    (lsp)))
 :config (setq lsp-pyright-python-executable-cmd "python"))

;; Flycheck
(use-package
 flycheck
 :init (global-flycheck-mode)
 :config
 (add-hook
  'python-mode-hook
  (lambda ()
    (setq flycheck-python-flake8-executable
          (executable-find "flake8"))
    (setq flycheck-python-pylint-executable
          (executable-find "pylint"))
    (setq flycheck-python-pycompile-executable
          (executable-find "python"))
    (flycheck-mode t))))

;; for Terraform
(use-package terraform-mode)
(use-package company-terraform :init (company-terraform-init))

;; for Ansible
(use-package ansible)
(use-package
 company-ansible
 :init (add-to-list 'company-backends 'company-ansible))

;; for copilot
; path must be specified when installed with nvm
(defun get-node-path ()
  "Find the path to Node.js binary."
  (let* ((base-dir (concat (getenv "HOME") "/.nvm/versions/node/"))
         (dirs (directory-files base-dir t "^v.*")))
    (when dirs
      (concat (car dirs) "/bin/node"))))

(setq copilot-node-executable
      (cond
       ((eq system-type 'windows-nt)
        "C:\\Program Files\\nodejs\\node.exe")
       ((eq system-type 'gnu/linux)
        (get-node-path))
       ((eq system-type 'darwin)
        "/usr/local/bin/node")
       (t
        "/usr/local/bin/node")))

; The following is required in the environment under the proxy Copilot-login is not possible
;(setq copilot-network-proxy '(:host "proxy" :port 3128))

; If you install copilot with straight, you can't find agent.js, so symbolic as follows
; [for linux/mac] cd /.emacs.d/elpa/copilot-20230605.35923 && ln -s /home/vagrant/.emacs.d/quelpa/build/copilot/dist
; [for win] mklink /D C:\Users\MINIS\AppData\Roaming\.emacs.d\straight\build\copilot\dist C:\Users\MINIS\AppData\Roaming\.emacs.d\straight\repos\copilot.el\dist
(use-package
 copilot
 :straight (copilot :type git :host github :repo "zerolfx/copilot.el")
 :config
 (defun my-tab ()
   (interactive)
   (or (copilot-accept-completion)
       (company-indent-or-complete-common nil)))
 (defun my-prog-mode-setup ()
   (local-set-key (kbd "TAB") #'my-tab)
   (local-set-key (kbd "<tab>") #'my-tab))
 (add-hook 'prog-mode-hook 'my-prog-mode-setup)
 (with-eval-after-load 'company
   (define-key company-active-map (kbd "TAB") #'my-tab)
   (define-key company-active-map (kbd "<tab>") #'my-tab)
   (define-key company-mode-map (kbd "TAB") #'my-tab)
   (define-key company-mode-map (kbd "<tab>") #'my-tab))
 ; when program mode copilot-mode enabled
 (add-hook 'prog-mode-hook 'copilot-mode))

(defun copilot-toggle ()
  "Toggle the GitHub Copilot on/off."
  (interactive)
  (if (bound-and-true-p copilot-mode)
      (progn
        (setq copilot-mode nil)
        (message "GitHub Copilot disabled."))
    (progn
      (setq copilot-mode t)
      (message "GitHub Copilot enabled."))))
(global-set-key (kbd "C-c c") 'copilot-toggle) ; C-c c copilot on/off

;;;;;;;;; Everything under this point was automatically added by Emacs.