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

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "/Users/tranthien/.emacs.d/images/dtmacs-logo.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
              (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

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

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(use-package which-key
  :straight (:build t)
  :defer t
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
    which-key-sort-order #'which-key-key-order-alpha
    which-key-allow-imprecise-window-fit nil
    which-key-sort-uppercase-first nil
    which-key-add-column-padding 1
    which-key-max-display-columns nil
    which-key-min-display-lines 6
    which-key-side-window-slot -10
    which-key-side-window-max-height 0.25
    which-key-idle-delay 0.8
    which-key-max-description-length 25
    which-key-allow-imprecise-window-fit nil
    which-key-separator " → " ))

(use-package which-key-posframe
  :ensure t
  :config
  (which-key-posframe-mode))

(use-package general
  :straight (:build t)
  :init

  (general-auto-unbind-keys)
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer dt/leader-keys
      :states '(normal insert visual emacs)
      :keymaps 'override
      :prefix "SPC" ;; set leader
      :global-prefix "M-SPC")) ;; access leader in insert mode

  (dt/leader-keys
      "SPC" '(counsel-M-x :wk "Counsel M-x")
      "." '(find-file :wk "Find file")
      "f c" '((lambda () (interactive) (find-file "~/.emacs.d/config.org")) :wk "Edit emacs config")
      "f r" '(counsel-recentf :wk "Find recent files")
      "TAB TAB" '(comment-line :wk "Comment lines"))

  (dt/leader-keys
      "d" '(:ignore t :wk "Dired")
      "d d" '(dired :wk "Open dired")
      "d j" '(dired-jump :wk "Dired jump to current")
      "d n" '(neotree-dir :wk "Open directory in neotree")
      "d p" '(peep-dired :wk "Peep-dired"))


  (dt/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  (dt/leader-keys
      "h r" '(:ignore t :wk "Reload")
      "h r r" '((lambda () (interactive)
                  (load-file "~/.emacs.d/init.el")
                  (ignore (elpaca-process-queues)))
              :wk "Reload emacs config"))

  (dt/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right"))

  (dt/leader-keys
      "s" '(:ignore t :wk "Search")
      "s f" '(swiper :wk "Search File"))

(use-package company
  :straight (:build t)
  :defer t
  :hook (company-mode . evil-normalize-keymaps)
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length     2
        company-toolsip-limit             14
        company-idle-delay                0.2
        company-tooltip-align-annotations t
        company-require-match             'never
        company-global-modes              '(not erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend ; always show candidates in overlay tooltip
          company-echo-metadata-frontend) ; show selected candidate docs in echo area
        company-backends '(company-capf)
        company-auto-commit         nil
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase    nil))

(use-package ivy
  :straight t
  :defer t
  :diminish
  :bind (("C-s" . swiper))
  :custom
      (setq ivy-use-virtual-buffers t)
      (setq ivy-count-format "(%d/%d) ")
      (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  (setq ivy-wrap                        t
        ivy-height                      17
        ivy-sort-max-size               50000
        ivy-fixed-height-minibuffer     t
        ivy-read-action-functions       #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        projectile-completion-system    'ivy
        ivy-on-del-error-function       #'ignore
        ivy-use-selectable-prompt       t))

(use-package ivy-prescient
  :after ivy
  :straight (:build t))

(use-package all-the-icons-ivy
  :straight (:build t)
  :after (ivy all-the-icons)
  :hook (after-init . all-the-icons-ivy-setup))

(use-package ivy-posframe
  :defer t
  :after (:any ivy helpful)
  :hook (ivy-mode . ivy-posframe-mode)
  :straight (:build t)
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-border-width   10
        ivy-posframe-parameters
        `((min-width  . 90)
          (min-height . ,ivy-height))))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :after counsel
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package counsel
  :straight (:build t)
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package projectile
  :straight (:build t)
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode)
  (add-to-list 'projectile-ignored-projects "~/")
  (add-to-list 'projectile-globally-ignored-directories "^node_modules$"))

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action)
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))
