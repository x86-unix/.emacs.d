;; パッケージ管理
; straight.el
(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil) ; package.elを無効化
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; use-package
(straight-use-package 'use-package)

;; 日本語環境とUTF-8の設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
; ターミナルを使用する場合のみ設定
(when (not (display-graphic-p))
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

;; UI改善
; Theme
(use-package gruvbox-theme :config (load-theme 'gruvbox-dark-hard t))
(set-face-foreground 'font-lock-comment-face "purple")

; nerd-iconsの設定
(use-package nerd-icons
  :ensure t
  :config
  (unless (or (file-exists-p "~/NFM.ttf")
              (file-exists-p "~/.local/share/fonts/NFM.ttf"))
    (nerd-icons-install-fonts)))

; dired-sidebarの設定
(use-package dired-sidebar
  :ensure t
  :bind (("C-c t" . dired-sidebar-toggle-sidebar))
  :config
  (setq dired-sidebar-theme 'nerd-icons))

; nerd-icons-diredの設定
(use-package nerd-icons-dired
  :ensure t
  :after dired
  :hook (dired-sidebar-mode . nerd-icons-dired-mode))

; Other Settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(global-display-line-numbers-mode) ; 行番号表示
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) ; スペースでインデント
(display-time-mode t)

;; バックアップ設定
(setq make-backup-files nil)       ; バックアップファイルを無効化
(setq auto-save-default nil)       ; 自動保存を無効化

;; 操作性
; Backspace even in mini buffer
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
; Resize Window
(defun window-resizer ()
  "Resize the selected window interactively."
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
        (let ((msg (format "Use [l/h/j/k] to resize, [q] to quit: %dx%d"
                           (window-width) (window-height))))
          (minibuffer-message msg))
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
         ((= c ?q)
          (minibuffer-message "Resize quit")
          (throw 'end-flag t)))))))
(global-set-key (kbd "C-c w") 'window-resizer)

; ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window))

; Toggle behavior of word wrap
(setq-default truncate-lines nil)

(defun toggle-truncate-lines ()
  "Toggle truncat lines"
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(global-set-key (kbd "C-c l") 'toggle-truncate-lines) ; ON/OFF

;; その他機能拡張
; open-junk-file
(use-package
 open-junk-file
 :config
 (setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S")
 :bind ("C-c j" . open-junk-file))

