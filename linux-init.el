;; font
; fonts install
(defun install-hack-font ()
  "Download and install the Hack font."
  (let* ((version "v3.003")  ;; バージョンをここで定義
         (font-url (format "https://github.com/source-foundry/Hack/releases/download/%s/Hack-%s-ttf.zip" version version))
         (download-dir "/tmp")
         (zip-file (expand-file-name "Hack.zip" download-dir))
         (extract-dir (expand-file-name "Hack" download-dir))
         (font-dir (expand-file-name "~/.local/share/fonts")))

    ;; フォントファイルのダウンロード
    (url-copy-file font-url zip-file t)

    ;; zipファイルを解凍
    (when (file-exists-p zip-file)
      (unless (file-exists-p extract-dir)
        (make-directory extract-dir))
      (call-process "unzip" nil nil nil zip-file "-d" extract-dir))

    ;; フォントファイルをコピー
    (dolist (font-file (directory-files extract-dir t "\\.ttf$"))
      (copy-file font-file font-dir t))

    ;; フォントキャッシュの更新
    (shell-command "fc-cache -f -v")))
; フォントをインストールする関数を呼び出す
(install-hack-font)

; font setting
(set-frame-font "Hack")
(add-to-list 'default-frame-alist '(font . "Hack-12")

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

