;; shell
; shell-pop
(use-package shell-pop
  :init
  ;; Windows用のシェルの設定
  (setq shell-pop-shell-type
        '("powershell"
          "*PowerShell*"
          (lambda () (powershell)))) ;; PowerShellを使用する場合
  ;; (setq shell-pop-shell-type
  ;;       '("cmd"
  ;;         "*Command Prompt*"
  ;;         (lambda () (ansi-term "cmd.exe")))) ;; cmdを使用する場合
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

