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