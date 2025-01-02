;; default-directory
(setq default-directory "~/")

;; UI
; nerd-icons-install-fonts
(use-package nerd-icons
  :ensure t
  :config
  (let ((font-dest (expand-file-name "~/.local/share/fonts/")))
    (unless (file-exists-p (expand-file-name "NFM.ttf" font-dest))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) t)))
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
; shell-pop
(use-package shell-pop
  :init
  (setq shell-pop-shell-type
        '("ansi-term"
          "*ansi-term*"
          (lambda () (ansi-term "/bin/bash"))))
  (setq shell-pop-full-span t)
  (setq shell-pop-window-size 20)
  :bind (("C-c s" . shell-pop))
  :config
  (defun my/shell-mode-setup ()
    (define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)
    (define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)
    (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
    (define-key shell-mode-map (kbd "C-n") 'comint-next-input))
  (add-hook 'shell-mode-hook 'my/shell-mode-setup))

;; term-mode でのキーバインド設定
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-c t") 'dired-sidebar-toggle-sidebar)))

;; font
; フォントキャッシュの更新関数
(defun update-font-cache ()
  "Update the font cache."
  (start-process "font-cache-update" nil "fc-cache" "-f" "-v")
  (message "Font cache updated."))

(defun install-hack-font ()
  "Download and install the Hack font if not already installed."
  (let* ((version "v3.003")
         (font-url (format "https://github.com/source-foundry/Hack/releases/download/%s/Hack-%s-ttf.zip" version version))
         (download-dir "/tmp")
         (zip-file (expand-file-name "Hack.zip" download-dir))
         (extract-dir (expand-file-name "Hack" download-dir))
         (font-dir (expand-file-name "~/.local/share/fonts"))
         (ttf-dir (expand-file-name "ttf" extract-dir))
         (fonts-installed (directory-files font-dir nil "\\.ttf$")))

    ;; フォントがすでにインストールされているか確認
    (if (cl-some (lambda (font) (string-match "Hack" font)) fonts-installed)
        (message "Hack font is already installed.")
      (progn
        ;; フォントファイルのダウンロード
        (url-copy-file font-url zip-file t)

        ;; zipファイルを解凍
        (when (file-exists-p zip-file)
          (unless (file-exists-p extract-dir)
            (make-directory extract-dir))
          (call-process "unzip" nil nil nil zip-file "-d" extract-dir))

        ;; ttfフォルダからフォントファイルをコピー
        (when (file-exists-p ttf-dir)
          (dolist (font-file (directory-files ttf-dir t "\\.ttf$"))
            (let ((font-name (file-name-nondirectory font-file)))
              (copy-file font-file (expand-file-name font-name font-dir) t))))))))

; フォントをインストールする関数を呼び出す
(install-hack-font)

; fontの設定
(set-frame-font "Hack")
(add-to-list 'default-frame-alist '(font . "Hack-12"))

; フォントキャッシュの更新
(update-font-cache)
