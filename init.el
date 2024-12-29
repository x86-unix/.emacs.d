;; "Use Emacs v29 with a GUI"

;; for Windows: C:\Users\ユーザー名\AppData\Roaming\.emacs.d
; runemacs.exe

;; 共通設定の読み込み
(when (file-exists-p "~/.emacs.d/common-init.el")
  (load-file "~/.emacs.d/common-init.el"))

;; OS 別設定の読み込み
(cond
 ((eq system-type 'gnu/linux)
  (when (file-exists-p "~/.emacs.d/linux-init.el")
    (load-file "~/.emacs.d/linux-init.el")))

 ((eq system-type 'darwin)
  (when (file-exists-p "~/.emacs.d/macos-init.el")
    (load-file "~/.emacs.d/macos-init.el")))

 ((eq system-type 'windows-nt)
  (when (file-exists-p "~/.emacs.d/windows-init.el")
    (load-file "~/.emacs.d/windows-init.el"))))