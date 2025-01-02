;; default-directory
(setq default-directory (getenv "USERPROFILE"))

;; UI
; nerd-icons-install-fonts
(defun my-nerd-icons-install-fonts (original-fun &optional pfx)
  "Override `nerd-icons-install-fonts` to specify the font installation directory for Windows."
  (let* ((url-format "https://raw.githubusercontent.com/rainstormstudio/nerd-icons.el/main/fonts/%s")
         (font-dest (concat (getenv "USERPROFILE") "\\AppData\\Local\\share\\fonts\\" nerd-icons-fonts-subdirectory))
         (known-dest? (stringp font-dest)))

    (unless (file-directory-p font-dest) (mkdir font-dest t))

    (mapc (lambda (font)
            (url-copy-file (format url-format font) (expand-file-name font font-dest) t))
          nerd-icons-font-names)

    (when known-dest?
      (message "Fonts downloaded. Please restart your application to see the changes."))

    (message "%s Successfully %s `nerd-icons' fonts to `%s'!"
             (nerd-icons-wicon "nf-weather-stars" :v-adjust 0.0)
             (if known-dest? "installed" "downloaded")
             font-dest)))

(advice-add 'nerd-icons-install-fonts :around #'my-nerd-icons-install-fonts)

(use-package nerd-icons
  :ensure t
  :config
  (let ((font-dest (concat (getenv "USERPROFILE") "\\AppData\\Local\\share\\fonts\\" nerd-icons-fonts-subdirectory)))
    (unless (file-exists-p (expand-file-name "NFM.ttf" font-dest))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) t))) ; 自動で'yes'返答
        (nerd-icons-install-fonts)))))

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

;; shell
; shell-popの設定
(use-package shell-pop
  :init
  (setq shell-pop-shell-type
        '("shell"
          "*Shell*"
          (lambda () (shell))))  ;; shellを使用
  (setq shell-pop-full-span t)
  (setq shell-pop-window-size 20) ;; ウィンドウサイズを20%に設定
  :bind (("C-c s" . shell-pop))
  :config
  (defun my/shell-mode-setup ()
    (define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)
    (define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)
    (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
    (define-key shell-mode-map (kbd "C-n") 'comint-next-input)
    ;; 空白行を出力しないようにする
    (setq comint-use-prompt-regexp nil)
    (setq comint-input-ignoredups t)
    (setq comint-process-echoes t))  ;; コマンドをエコーしない
  (add-hook 'shell-mode-hook 'my/shell-mode-setup))

;; term-mode でのキーバインド設定
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-c t") 'dired-sidebar-toggle-sidebar)))

;; font
; fontの設定
(set-frame-font "BIZ UDゴシック")
(add-to-list 'default-frame-alist '(font . "BIZ UDゴシック-12"))

; ユーザーフォントのインストール
; PowerShellスクリプトを実行する関数
(defun run-powershell-script (script-path fonts-path)
  (start-process "powershell" nil "pwsh.exe" "-ExecutionPolicy" "Bypass" "-File" script-path fonts-path))

(let ((script-path (concat (getenv "USERPROFILE") "\\AppData\\Roaming\\.emacs.d\\InstallFonts.ps1"))
      (fonts-path (concat (getenv "USERPROFILE") "\\AppData\\Local\\share\\fonts\\")))
  ;; スクリプト実行
  (run-powershell-script script-path fonts-path))
