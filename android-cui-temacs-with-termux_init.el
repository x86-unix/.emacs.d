;; Tramp fail timeout
(setq tramp-connection-timeout 5)

;; initial install packages
(defvar my-install-package-list
  '(
    use-package
    )
  "Install Initial Packages")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg my-install-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Load-Path
; Auto add load-path recursively
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path) (normal-top-level-add-subdirs-to-load-path))))))
; Emacs on Android - use with Termux
(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))

;; Specify the load path under elpa
;(add-to-load-path "elpa" "snippets")
(add-to-load-path "elpa")

;; use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; quelpa-use-package
(use-package quelpa-use-package)

;; UTF-8
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; bar
(tool-bar-mode 0)

;; Display full path of file in title bar
(setq frame-title-format "%f")

;; no messages
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; mode line
(display-time-mode t)

;; assign C-h to Backspace
; Backspace even in mini buffer
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; disable beep sound
(setq visible-bell t)

;; Highlight tabs, double-byte spaces, etc.
(global-whitespace-mode t)

;; Toggle behavior of word wrap
(setq-default truncate-lines t)
(defun toggle-truncate-lines ()
  "Toggle truncat lines"
  (interactive)
  (if truncate-lines
    (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))
(global-set-key (kbd "C-c l") 'toggle-truncate-lines) ; ON/OFF

;; Ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window))

;; Resize Window
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t (message "Quit")
               (throw 'end-flag t)))))))
(global-set-key (kbd "C-c w") 'window-resizer)

;; Tab
(setq-default tab-width 4 indent-tabs-mode nil)

;; Automatic backup settings
; Collect backup files and autosave files into ~/.emacs.d/backups/
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
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
      (setq display-line-numbers 'relative)
    )
  ;; Use `global-linum-mode` for versions earlier than 26.1
  (global-linum-mode)
)

;; Theme
(use-package doom-themes
  :config
  ; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)  ; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Blink corresponding paren
(show-paren-mode 1)

;; visualize indent
(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'column))

;; dired
; recentf-ext
(use-package recentf-ext
  :config
  (setq recentf-max-saved-items 100) ; Save up to 100 as history
  :bind
  ("C-c n" . recentf-open-files)
)

;; open-junk-file
(use-package open-junk-file
  :config
  (setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S")
  :bind
  ("C-c j" . open-junk-file))

;; neotree
(use-package neotree
  :bind (("C-c t" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)) ; Use icons if graphic display is possible, otherwise use arrows
  (setq neo-show-hidden-files t)) ; Show hidden files by default

;; company
(use-package company
  :init
  :config
  (setq company-dabbrev-downcase nil)
  ; without delay.
  (setq company-idle-delay 0)
  ; Default is 4. Make completion start from fewer characters.
  (setq company-minimum-prefix-length 2)
  ; If you try to go further down at the bottom of the candidate, it will return to the top.
  (setq company-selection-wrap-around t)
  ; Show number.
  (setq company-show-numbers t)
  (global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-s" . company-filter-candidates)
              ("<tab>" . company-complete-selection))
  :bind (:map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;; disable-company-mode lists
(defvar my-disable-company-modes
  '(shell-mode
    eshell-mode
    term-mode
    )
  )
(defun my-disable-company-in-selected-modes ()
  (when (apply 'derived-mode-p my-disable-company-modes)
    (company-mode -1)))
(mapc (lambda (mode)
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  'my-disable-company-in-selected-modes))
      my-disable-company-modes)

;; company-shell
(use-package company-shell
  :config
  (add-to-list 'company-backends 'company-shell))

;; electric-pair
(use-package electric
  :init
  (progn
    (electric-pair-mode 1)))

;; shell-mode
(defun shell-in-split-window ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (if (not (get-buffer "*shell*"))
      (progn
        (shell)
        (other-window 1))
    (switch-to-buffer-other-window "*shell*")))

(global-set-key (kbd "C-c s") 'shell-in-split-window)

;; black
(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

;; regular expression support
(use-package regex-tool
  :bind (("C-c r" . regex-tool)))

;; for python
; Need pip install virtualenv in advance (common to mac,windows,linux)
(setq python-indent-offset 4)
; company-jedi
(defun my/install-jedi-server-if-needed ()
  "Check if jedi server is installed. If not, install it."
  (let* ((default-dir (cond
                       ((eq system-type 'windows-nt)
                        (concat (getenv "USERPROFILE") "/AppData/Roaming/.emacs.d/.python-environments/default"))
                       ((eq system-type 'darwin)
                        "~/.emacs.d/.python-environments/default/")
                       (t
                        "~/.emacs.d/.python-environments/default/")))
         (jedi-dir (expand-file-name default-dir)))
    (unless (file-exists-p jedi-dir)
      (jedi:install-server))))

(use-package company-jedi
  :commands company-jedi
  :init
  (my/install-jedi-server-if-needed)
  (defun use-package-company-add-company-jedi ()
    (unless (member 'company-jedi company-backends)
      (add-to-list 'company-backends 'company-jedi)))
  (add-hook 'python-mode-hook 'use-package-company-add-company-jedi))

;; Flycheck
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq flycheck-python-flake8-executable (executable-find "flake8"))
              (setq flycheck-python-pylint-executable (executable-find "pylint"))
              (setq flycheck-python-pycompile-executable (executable-find "python"))
              (flycheck-mode t))))

;; for Terraform
(use-package terraform-mode)
(use-package company-terraform
  :init
  (company-terraform-init))

;; for Ansible
(use-package ansible)
(use-package company-ansible
  :init
  (add-to-list 'company-backends 'company-ansible))

;;;;;;;;; Auto generated