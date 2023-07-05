;; Tramp fail timeout
(setq tramp-connection-timeout 5)

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
; Emacs on Android - use with Termux
(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))

;; Specify the load path under elpa
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

;; reloading the file
(global-set-key (kbd "C-c f")
                (lambda ()
                  (interactive)
                  (revert-buffer :ignore-auto :noconfirm)
                  (message "file reloaded.")))

;; assign C-h to Backspace
; Backspace even in mini buffer
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; disable beep sound
(setq visible-bell t)

;; Highlight whitespace
(use-package
 whitespace
 :init
 (global-whitespace-mode +1) ; Enable whitespace-mode by default
 :config
 (setq whitespace-style '(face spaces space-mark)) ; Only visualize spaces

 ;; Set visualizations
 (setq
  whitespace-display-mappings
  '((space-mark ?\u0020 [?\u0020]) ; visualization of half-width space (no mark, only color)
    (space-mark ?\u3000 [?\u25A1]))) ; visualization of full-width space

 ;; Set the color for each type of whitespace
 (set-face-attribute 'whitespace-space nil :background "gray20") ; Half-width space in lightgray background
 (set-face-attribute 'whitespace-hspace nil :background "yellow")) ; Full-width space in yellow background

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

;; Ace-window
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

;; dired-sidebar
(use-package
 dired-sidebar
 :commands (dired-sidebar-toggle-sidebar)
 :bind (("C-c t" . dired-sidebar-toggle-sidebar))
 :config
 (setq dired-sidebar-show-hidden-files t)
 (setq dired-sidebar-width 20))

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

;; electric-pair
(use-package
 electric
 :init
 (progn
   (electric-pair-mode 1)))

;; shell-mode
(defun shell-in-split-window ()
  (interactive)
  (let*
      ((window-height-ratio 0.8) ; Specify the desired height ratio (e.g., 0.5 for 50%)
       (window-height (floor (* window-height-ratio (frame-height)))))
    (split-window-below window-height))
  (other-window 1)
  (if (not (get-buffer "*shell*"))
      (progn
        (shell)
        (other-window 1))
    (switch-to-buffer-other-window "*shell*")))

(defun my/shell-mode-setup ()
  (define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)
  (define-key
   shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)
  (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
  (define-key shell-mode-map (kbd "C-n") 'comint-next-input))

(add-hook 'shell-mode-hook 'my/shell-mode-setup)

(global-set-key (kbd "C-c s") 'shell-in-split-window)

;; elisp-autofmt
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

;; for python
; Need pip install virtualenv in advance (common to mac,windows,linux)
(setq python-indent-offset 4)
; company-jedi
(defun my/install-jedi-server-if-needed ()
  "Check if jedi server is installed. If not, install it."
  (let*
      ((default-dir
        (cond
         ((eq system-type 'windows-nt)
          (concat
           (getenv "USERPROFILE")
           "/AppData/Roaming/.emacs.d/.python-environments/default"))
         ((eq system-type 'darwin)
          "~/.emacs.d/.python-environments/default/")
         (t
          "~/.emacs.d/.python-environments/default/")))
       (jedi-dir (expand-file-name default-dir)))
    (unless (file-exists-p jedi-dir)
      (jedi:install-server))))

(use-package
 company-jedi
 :commands company-jedi
 :init (my/install-jedi-server-if-needed)
 (defun use-package-company-add-company-jedi ()
   (unless (member 'company-jedi company-backends)
     (add-to-list 'company-backends 'company-jedi)))
 (add-hook 'python-mode-hook 'use-package-company-add-company-jedi))

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
(setq copilot-node-executable
      "/data/data/com.termux/files/usr/bin/node")

; If you install copilot with quelpa, you can't find agent.js, so symbolic as follows
; cd  /.emacs.d/elpa/copilot-20230605.35923 && \
; ln -s /home/vagrant/.emacs.d/quelpa/build/copilot/dist
(use-package
 copilot
 :quelpa (copilot :fetcher github :repo "zerolfx/copilot.el")
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
