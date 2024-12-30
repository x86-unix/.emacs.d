;; font
; fonts install
(defun install-hack-font ()
  "Download and install the Hack font."
  (let* ((version "v3.003")  ;; バージョン定義
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

    ;; フォントファイルを再帰的に検索してコピー
    (let ((found-ttf-files nil))
      (dolist (dir (list extract-dir))
        (setq found-ttf-files
              (append found-ttf-files
                      (directory-files dir t "\\.ttf$" t)))
        ;; サブディレクトリを再帰的に探索
        (dolist (subdir (directory-files dir t "[^.]"))
          (when (file-directory-p subdir)
            (setq found-ttf-files
                  (append found-ttf-files
                          (directory-files subdir t "\\.ttf$" t)))))))

      ;; 見つかったフォントファイルをコピー
      (dolist (font-file found-ttf-files)
        (copy-file font-file font-dir t)))

    ;; フォントキャッシュの更新
    (shell-command "fc-cache -f -v")))

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

