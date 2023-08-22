(setq debug-on-error t)
;; Initialize package sources
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                    ("gnu"    . "https://elpa.gnu.org/packages/")
                    ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(add-to-list 'load-path "~/.emacs.d/scripts/")

(require 'straight-setup) ;; The Straight Package Manager
(require 'buffer-move) ;; Buffer-move for better window management
(setq straight-vc-git-default-remote-name "straight")

(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))

(setq straight-vc-git-default-remote-name "straight")

(straight-use-package '(use-package :build t))
(setq use-package-always-ensure t)

(add-hook 'before-save-hook #'whitespace-cleanup)
(server-start)

(setq-default sentence-end-double-space nil)

(setq-default initial-major-mode 'emacs-lisp-mode)

(setq-default tab-width 2)

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file) ; Don’t forget to load it, we still need it
  (load custom-file))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode 1)

(unless (package-installed-p 'autothemer)
  (package-refresh-contents)
  (package-install 'autothemer))
  
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

(use-package doom-themes
  :straight (:build t)
  :ensure t
  :config
  ;; (load-theme 'catppuccin-latte t)
  ;; (load-theme 'catppuccin-frappe t)
  (load-theme 'catppuccin-macchiato t)
  ;; (load-theme 'catppuccin-mocha t)
  ;; (load-theme 'rose-pine t)
  ;; (load-theme 'oxocarbon t)
  ;; (load-theme 'kman t)
  ;; (load-theme 'kanagawa t)
  ;; (load-theme 'doom-tokyo-night t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    ;; :font "Victor Mono"
                    :weight 'regular
                    :height 135)

;;Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    ;; :font "Victor Mono"
                    :weight 'regular
                    :height 135)

;;Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    ;; :font "Victor Mono"
                    ;; :font "Cantarell"
                    :font "Victor Mono"
                    :height 135
                    :weight 'light)

;;(set-fontset-font t 'symbol "Noto Color Emoji")
;;(set-fontset-font t 'symbol "Symbola" nil 'append)

(use-package emojify
  :straight (:build t)
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-emojis-dir (concat user-emacs-directory "emojify/"))
  (emojify-display-style 'image)
  (emojify-download-emojis-p t)
  :config
  (global-emojify-mode 1))

(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t
          evil-want-keybinding nil
          evil-want-C-u-scroll t
          evil-want-C-i-jump nil
          evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
    (evil-mode))
  (use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))
  (use-package evil-tutor)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Source Code Pro" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "/Users/tranthien/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
