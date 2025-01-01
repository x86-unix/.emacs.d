;; font
; fontの設定
(set-frame-font "Osaka")
(add-to-list 'default-frame-alist '(font . "Osaka-12"))

;; UI
; nerd-iconsの設定
(use-package nerd-icons
  :ensure t
  :config
  (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
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

;; shell
; shell-popの設定
(use-package shell-pop
  :init
  (setq shell-pop-shell-type
        '("ansi-term"
          "*ansi-term*"
          (lambda () (ansi-term "/bin/zsh"))))
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
