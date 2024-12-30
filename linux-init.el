;; font
; install font
(defun install-hack-font ()
  "Download and install the Hack font if not already installed."
  (let* ((version "v3.003")  ;; バージョンをここで定義
         (font-url (format "https://github.com/source-foundry/Hack/releases/download/%s/Hack-%s-ttf.zip" version version))  ;; URLを動的に生成
         (download-dir "/tmp")
         (zip-file (expand-file-name "Hack.zip" download-dir))
         (extract-dir (expand-file-name "Hack" download-dir))
         (font-dir (expand-file-name "~/.local/share/fonts"))
         (ttf-dir (expand-file-name "ttf" extract-dir))  ;; ttfディレクトリのパスを指定
         (fonts-installed (directory-files font-dir nil "\\.ttf$")))  ;; インストール済みフォントをチェック

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
        (when (file-exists-p ttf-dir)  ;; ttfディレクトリが存在するか確認
          (dolist (font-file (directory-files ttf-dir t "\\.ttf$"))  ;; .ttfファイルをリストアップ
            (let ((font-name (file-name-nondirectory font-file)))  ;; ファイル名を取得
              (copy-file font-file (expand-file-name font-name font-dir) t))))  ;; フォントをコピー

        ;; フォントキャッシュの更新
        (shell-command "fc-cache -f -v")
        (message "Hack font installed and cache updated.")))))

; フォントをインストールする関数を呼び出す
(install-hack-font)

; font setting
(set-frame-font "Hack")
(add-to-list 'default-frame-alist '(font . "Hack-12"))

;; shell
; shell-pop
(use-package shell-pop
  :init
  (setq shell-pop-shell-type
        '("ansi-term"
          "*ansi-term*"
          (lambda () (ansi-term "/bin/bash"))))
  (setq shell-pop-full-span t)
  (setq shell-pop-window-size 20) ;; Specify window size as 20% of screen height
  :bind (("C-c s" . shell-pop))
  :config
  (defun my/shell-mode-setup ()
    (define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)
    (define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)
    (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
    (define-key shell-mode-map (kbd "C-n") 'comint-next-input))
  (add-hook 'shell-mode-hook 'my/shell-mode-setup))

