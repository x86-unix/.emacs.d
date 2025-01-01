;; disable beep sound
(setq visible-bell t)

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

;; UI
; Theme
(use-package gruvbox-theme :config (load-theme 'gruvbox-dark-hard t))
(set-face-foreground 'font-lock-comment-face "purple")
; smartparens 
(use-package
 smartparens
 :config (require 'smartparens-config) (smartparens-global-mode t))
; Other Settings
(menu-bar-mode -1) ; メニューバーを非表示にする
(tool-bar-mode -1) ; ツールバーを非表示にする
(scroll-bar-mode -1) ; スクロールバーを非表示にする
(setq inhibit-startup-screen t) ; スタートアップスクリーンを表示しない
(global-display-line-numbers-mode) ; 行番号を表示する
(setq-default tab-width 4) ; タブの幅を4スペースに設定
(setq-default indent-tabs-mode nil) ; インデントをスペースで行う
(display-time-mode t) ; 時刻を表示する

; 空白等の表示
(use-package whitespace
  :ensure t
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode))
  :custom
  (whitespace-style '(face
                      tabs
                      spaces
                      newline
                      space-mark
                      tab-mark
                      newline-mark))
  (whitespace-space-regexp "\\(\u3000\\| \\)")
  (whitespace-display-mappings
   '((space-mark   ?\u3000 [?\u25A1] [?_]) ; 全角スペースを四角で表示
     (space-mark   ?\s [?\u00B7])          ; 半角スペースを中点で表示
     (newline-mark ?\n [?\u2193 ?\n])       ; 改行を下矢印で表示
     (tab-mark     ?\t [?\u2192 ?\t])))     ; タブを矢印で表示
  (whitespace-action '(auto-cleanup))
  :config
  (setq whitespace-global-modes '(not org-mode)))

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

; ファイル更新自動リロード機能
(global-auto-revert-mode t)

;; 機能拡張
; open-junk-file
(use-package open-junk-file
  :config
  (setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S")
  :bind ("C-c j" . open-junk-file))

; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode)  ;; Flycheckを全体で有効化
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))  ;; 自動チェックの設定
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))  ;; 無効にするチェックを指定
  (setq flycheck-idle-change-delay 0.5)  ;; チェックの遅延時間を設定
)