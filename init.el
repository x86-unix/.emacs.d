;; tramp fail timeout
(setq tramp-connection-timeout 5)

;; Window size
(setq initial-frame-alist
  (append (list
  '(width . 100) ;; ウィンドウ幅
  '(height . 55) ;; ウィンドウ高さ
  '(top . 30) ;; 表示位置
  '(left . 1100) ;; 表示位置
  )
  initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

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
; load-pathを再帰的に自動追加
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path) (normal-top-level-add-subdirs-to-load-path))))))

;; elpa配下をロードパスに指定
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

;; font
(when (eq system-type 'gnu/linux) ; for Linux
  (set-frame-font "Ricty")
  (add-to-list 'default-frame-alist '(font . "Ricty-12")))

;; bar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format "%f")

;; no messages
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; mode line
(display-time-mode t)

;; C-h を Backspace へ割当てる
; mini buffer内でもbackspaceする
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; on mouse yank
(setq mouse-drag-copy-region t)

;; disable beep sound
(setq visible-bell t)

;; Tabや全角空白などを強調表示
(global-whitespace-mode t)

(setq whitespace-style
  '(face tabs tab-mark spaces space-mark newline newline-mark))
(setq whitespace-display-mappings
  '((space-mark ?\u3000 [?□])  ; 全角スペース形状
   ;(space-mark ?\u0020 [?\xB7])  ; 半角スペース形状
   (newline-mark ?\n   [?↓?\n]) ; 改行記号形状
   (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t]) ; TAB形状
   )
)

;; 折り返し表示のトグル動作
; デフォルトは折り返ししない 
(setq-default truncate-lines t)
(defun toggle-truncate-lines ()
  "Toggle truncat lines"
  (interactive)
  (if truncate-lines
    (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))
(global-set-key (kbd "C-c l") 'toggle-truncate-lines) ; ON/OFF

;; ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window))

;; Windowサイズ変更
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

;; 自動バックアップ設定
; バックアップファイルとオートセーブファイルを ~/.emacs.d/backups/ へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

; フレームタイトルの設定
(setq frame-title-format "%b")
; バックアップのバージョン管理を行う
(setq version-control t)
; 新しいものをいくつ残すか
(setq kept-new-versions 10)
; 古いものをいくつ残すか
(setq kept-old-versions 10)
; 古いバージョンを消去するのに確認を求めない。
(setq delete-old-versions t)

;; 行番号の表示
(global-linum-mode t)
(setq linum-format "%3d  ")

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

;; dired 最近開いたファイル
; recentf-ext
(use-package recentf-ext
  :config
  (setq recentf-max-saved-items 100) ; 100個まで履歴として保存 
  :bind
  ("C-c n" . recentf-open-files)
)

;; open-junk-file
(use-package open-junk-file
  :config
  (setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S")
  :bind
  ("C-c j" . open-junk-file))

;; all-the-icons
; neotree などで必要なフォント
; M-x all-the-icons-install-fonts でフォントを手動インストールする必要がある
(use-package all-the-icons
  :if (display-graphic-p))

(use-package neotree
  :bind (("C-c t" . neotree-toggle))  ;; C-c t で neotree を開閉
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))  ;; グラフィック表示が可能ならアイコンを、そうでなければ矢印を使用

;; company
(use-package company
  :init
  :config
  (setq company-dabbrev-downcase nil)
  ; 遅延なしにする。
  (setq company-idle-delay 0)
  ; デフォルトは4。より少ない文字数から補完が始まる様にする。
  (setq company-minimum-prefix-length 2)
  ; 候補の一番下でさらに下に行こうとすると一番上に戻る。
  (setq company-selection-wrap-around t)
  ; 番号を表示する。
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

;; company-quickhelp
(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode t))

;; company-quickhelp
(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode t))

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

;; shell-pop
(use-package shell-pop
  :init
  (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
  (setq shell-pop-term-shell "/bin/bash")
  (setq shell-pop-full-span t)
  :bind (("C-c s" . shell-pop)))

;; black
(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

;; 正規表現支援
(use-package regex-tool
  :bind (("C-c r" . regex-tool)))

;; for python
;事前に pip install virtualenv が必要 (mac,windows,linux共通)
;company-jedi
(defun my/install-jedi-server-if-needed ()
  "Check if jedi server is installed. If not, install it."
  (let* ((default-dir (cond
                       ((eq system-type 'windows-nt)
                        "~/AppData/Roaming/.emacs.d/.python-environments/default")
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

;; for copilot
; nvm でインストールした場合path指定
; sample. Windows
; (setq copilot-node-executable "C:\\Program Files\\nodejs\\node.exe")
(setq copilot-node-executable "/home/vagrant/.nvm/versions/node/v16.20.0/bin/node")
; proxy配下の環境では以下が必要 copilot-loginができない
(setq copilot-network-proxy
      '(:host "192.xxx.xxx.xxx" :port 3128))

; quelpa で copilotをインストールすると、agent.jsを見つけられないので、以下のようにシンボリックを張る
; cd  /.emacs.d/elpa/copilot-20230605.35923 && \
; ln -s /home/vagrant/.emacs.d/quelpa/build/copilot/dist
(use-package copilot
  :quelpa
  (copilot :fetcher github :repo "zerolfx/copilot.el")
  :config
  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)))
  (global-set-key (kbd "C-TAB") #'my-tab)
  (global-set-key (kbd "C-<tab>") #'my-tab)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-TAB") #'my-tab)
    (define-key company-active-map (kbd "C-<tab>") #'my-tab)
    (define-key company-mode-map (kbd "C-TAB") #'my-tab)
    (define-key company-mode-map (kbd "C-<tab>") #'my-tab))
    ; プログラムモードの場合、copilot-modeを実行
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

(global-set-key (kbd "C-c p") 'copilot-toggle) ; C-c p copilot on/off

;;;;;;;;; Auto generated 
