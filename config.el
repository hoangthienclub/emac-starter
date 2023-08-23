(setq debug-on-error t)
;; Initialize package sources
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                    ("gnu"    . "https://elpa.gnu.org/packages/")
                    ("melpa-stable"   .  "https://stable.melpa.org/packages/")
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

(defmacro csetq (&rest forms)
  "Bind each custom variable FORM to the value of its VAL.

FORMS is a list of pairs of values [FORM VAL].
`customize-set-variable' is called sequentially on each pair
contained in FORMS. This means `csetq' has a similar behavior as
`setq': each VAL expression is evaluated sequentially, i.e., the
first VAL is evaluated before the second, and so on. This means
the value of the first FORM can be used to set the second FORM.

The return value of `csetq' is the value of the last VAL.

\(fn [FORM VAL]...)"
  (declare (debug (&rest sexp form))
           (indent 1))
  ;; Check if we have an even number of arguments
  (when (= (mod (length forms) 2) 1)
    (signal 'wrong-number-of-arguments (list 'csetq (1+ (length forms)))))
  ;; Transform FORMS into a list of pairs (FORM . VALUE)
  (let (sexps)
    (while forms
      (let ((form  (pop forms))
            (value (pop forms)))
        (push `(customize-set-variable ',form ,value)
              sexps)))
    `(progn ,@(nreverse sexps))))

(defun add-all-to-list (list-var elements &optional append compare-fn)
  "Add ELEMENTS to the value of LIST-VAR if it isn’t there yet.

ELEMENTS is a list of values. For documentation on the variables
APPEND and COMPARE-FN, see `add-to-list'."
  (let (return)
    (dolist (elt elements return)
      (setq return (add-to-list list-var elt append compare-fn)))))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
        (lambda ()
        (message "*** Emacs loaded in %s with %d garbage collections."
                    (format "%.2f seconds"
                            (float-time
                            (time-subtract after-init-time before-init-time)))
                    gcs-done)))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(setq user-full-name       "Tran Hoang Thien"
      user-real-login-name "Tran Hoang Thien"
      user-login-name      "hoangthienclub"
      user-mail-address    "thien301194@gmail.com")

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

(setq evil-insert-state-cursor '((bar . 2) "orange")
      evil-normal-state-cursor '(box "orange"))

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

(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-height 35)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-bar-inactive nil)
  :config
  ;; FIX Add some padding to the right
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug
      repl lsp minor-modes input-method indent-info buffer-encoding major-mode
      process vcs checker time "   ")))
(setq evil-normal-state-tag   (propertize "[Normal]" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag    (propertize "[Emacs]" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag   (propertize "[Insert]" 'face '((:background "red") :foreground "white"))
      evil-motion-state-tag   (propertize "[Motion]" 'face '((:background "blue") :foreground "white"))
      evil-visual-state-tag   (propertize "[Visual]" 'face '((:background "yellow" :foreground "black")))
      evil-operator-state-tag (propertize "[Operator]" 'face '((:background "purple"))))

(require 'time)
(setq display-time-format "%Y-%m-%d %H:%M")
(display-time-mode 1) ; display time in modeline

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package rainbow-delimiters
  :straight (:build t)
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hydra
  :straight (:build t)
  :defer t)

(defhydra windows-adjust-size ()
  "
^Zoom^                                ^Other
^^^^^^^-----------------------------------------
[_j_/_k_] shrink/enlarge vertically   [_q_] quit
[_l_/_h_] shrink/enlarge horizontally
"
  ("q" nil :exit t)
  ("l" shrink-window-horizontally)
  ("j" enlarge-window)
  ("k" shrink-window)
  ("h" enlarge-window-horizontally))

(use-package info-colors
  :straight (:build t)
  :commands info-colors-fnontify-node
  :hook (Info-selection . info-colors-fontify-node)
  :hook (Info-mode      . mixed-pitch-mode))

(use-package diminish)

(use-package olivetti
  :ensure t
  :config
  (setq olivetti-body-width 80) ; Set the width of the text column
  (setq olivetti-minimum-body-width 80) ; Set a minimum body width
  (setq olivetti-recall-visual-line-mode-entry-state t) ; Remember visual-line-mode
  (add-hook 'olivetti-mode-hook #'visual-line-mode)) ; Enable visual-line-mode

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

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
(evil-global-set-key 'motion "w" 'evil-avy-goto-word-1)

(use-package evil-collection
  :after evil
  :straight (:build t)
  :config
  (evil-collection-init))

(use-package undo-tree
  :defer t
  :straight (:build t)
  :diminish
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name (file-name-as-directory "undo-tree-hist")
                               user-emacs-directory))))
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff       t
        undo-tree-auto-save-history     t
        undo-tree-enable-undo-in-region t
        undo-limit        (* 800 1024)
        undo-strong-limit (* 12 1024 1024)
        undo-outer-limit  (* 128 1024 1024)))

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
  (setq visual-fill-column-width 150
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
  :init (which-key-mode)
  :diminish which-key-mode
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
  (general-create-definer dqv/underfine
      :keymaps 'override
      :states '(normal emacs))

  ;; set up 'SPC' as the global leader key
  (general-create-definer dqv/leader-key
      :states '(normal insert visual emacs)
      :keymaps 'override
      :prefix "SPC" ;; set leade
      :global-prefix "M-SPC")
  (general-create-definer dqv/evil
      :states '(normal)) ;; access leader in insert mode
  (general-create-definer dqv/major-leader-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "M-m"))

  (dqv/leader-key
      "SPC" '(counsel-M-x :wk "Counsel M-x")
      "." '(find-file :wk "Find file")
      "f c" '((lambda () (interactive) (find-file "~/.emacs.d/config.org")) :wk "Edit emacs config")
      "f r" '(counsel-recentf :wk "Find recent files")
      "TAB TAB" '(comment-line :wk "Comment lines"))

  (dqv/leader-key
      "h r" '(:ignore t :wk "Reload")
      "h t" '(load-theme :wk "Load theme")
      "h r r" '((lambda () (interactive)
                  (load-file "~/.emacs.d/init.el")
                  (ignore (elpaca-process-queues)))
              :wk "Reload emacs config"))

(dqv/leader-key
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired"))

(dqv/leader-key
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

(dqv/leader-key
  "t" '(:ignore t :wk "Toggle")
  "z" '(olivetti-mode :wk "Zen Mode")
  "t e" '(eshell-toggle :wk "Toggle eshell")
  "t f" '(flycheck-mode :wk "Toggle flycheck")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
  "t r" '(rainbow-mode :wk "Toggle rainbow mode")
  "t t" '(visual-line-mode :wk "Toggle truncated lines")
  "t v" '(vterm-toggle :wk "Toggle vterm"))

(dqv/leader-key
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(kill-buffer-and-delete-window :wk "Kill & Delete")
    "w d" '(delete-window :wk "Delete window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(split-window-below-and-focus :wk "Horizontal split window")
    "w v" '(split-window-right-and-focus :wk "Vertical split window")
    "w i" '(windows-adjust-size/body :wk "Window Size")
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

(dqv/leader-key
    "s" '(:ignore t :wk "Search")
    "s t" '(counsel-projectile-git-grep :wk "Search Text")
    "s f" '(swiper :wk "Search File"))

(dqv/leader-key
    "d" '(:ignore t :wk "LSP")
    "ll"  #'lsp
    "lm"  #'lsp-ui-imenu
    "ld"  #'xref-find-definitions-other-window
    "lD"  #'xref-find-definitions)

(dqv/leader-key
    "p" '(:ignore t:wl "Projectile")
    "p a" '(projectile-add-known-project :wk "Add Project")
    "p s" '(projectile-switch-project :wk "Switch Project")
    "p f" '(projectile-find-file :wk "Find File")
    "p r" '(projectile-remove-known-project :wk "Remove Known Project"))

(dqv/evil
  ;;:packages '(counsel)
    "s" '(window-configuration-to-register :wk "Register Window")
    "f" '(jump-to-register :wk "Jump Register")
    "K" '(lsp-ui-doc-toggle :wk "Show Document")
    "U"   #'evil-redo)

(use-package company
  :straight (:build t)
  :defer t
  :hook (company-mode . evil-normalize-keymaps)
  :diminish
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

(use-package company-dict
  :after company
  :straight (:build t)
  :config
  (setq company-dict-dir (expand-file-name "dicts" user-emacs-directory)))

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
  (ivy-mode 1 )
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
  :diminish
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

(use-package ripgrep
  :if (executable-find "rg")
  :straight (:build t)
  :defer t)

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

(use-package counsel-projectile
  :straight (:build t)
  :after (counsel projectile)
  :config (counsel-projectile-mode))

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

(use-package avy
  :defer t
  :straight t
  :config
  (setq avy-keys           '(?a ?u ?i ?e ?c ?t ?s ?r ?n)
         avy-dispatch-alist '((?x . avy-action-kill-move)
                              (?X . avy-action-kill-stay)
                              (?T . avy-action-teleport)
                              (?m . avy-action-mark)
                              (?C . avy-action-copy)
                              (?y . avy-action-yank)
                              (?Y . avy-action-yank-line)
                              (?I . avy-action-ispell)
                              (?z . avy-action-zap-to-char)))
    :general
    (dqv/evil
        :pakages 'avy
        "gc" #'evil-avy-goto-char-timer
        "gl" #'evil-avy-goto-line))

(use-package yasnippet
  :defer t
  :straight (:build t)
  :init
  (yas-global-mode)
  :diminish
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet
  :straight (:build t)
  :diminish)

(use-package yatemplate
  :defer t
  :after yasnippet
  :straight (:build t)
  :diminish)

(use-package ivy-yasnippet
  :defer t
  :after (ivy yasnippet)
  :straight (:build t)
  :diminish
  :general
  (dqv/leader-key
    :infix "i"
    :packages 'ivy-yasnippet
    "y" #'ivy-yasnippet))

(use-package editorconfig
  :defer t
  :straight (:build t)
  :diminish editorconfig-mode
  :config
  (editorconfig-mode t))

(use-package evil-nerd-commenter
  :after evil
  :straight (:build t))
(global-set-key (kbd "s-/") #'evilnc-comment-or-uncomment-lines)

(use-package string-edit-at-point
  :defer t
  :straight (:build t))

(use-package move-text
  :straight (:build t))

(global-set-key (kbd "s-j") #'move-text-down)
(global-set-key (kbd "s-k") #'move-text-up)

(use-package eyebrowse
  :straight (:build t)
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-mode 1))

(dqv/leader-key
 "TAB"  '(:ignore t :which-key "Window Management")
 "TAB 0" '(eyebrowse-switch-to-window-config-0 :which-key "Select Windown 0")
 "TAB 1" '(eyebrowse-switch-to-window-config-1 :which-key "Select Window 1")
 "TAB 2" '(eyebrowse-switch-to-window-config-2 :which-key "Select Window 2")
 "TAB 3" '(eyebrowse-switch-to-window-config-3 :which-key "Select Window 3")
 "TAB 4" '(eyebrowse-switch-to-window-config-4 :which-key "Select Window 4")
 "TAB 5" '(eyebrowse-switch-to-window-config-5 :which-key "Select Window 5")
 "TAB 6" '(eyebrowse-switch-to-window-config-6 :which-key "Select Window 6")
 "TAB 7" '(eyebrowse-switch-to-window-config-7 :which-key "Select Window 7")
 "TAB 8" '(eyebrowse-switch-to-window-config-8 :which-key "Select Window 8")
 "TAB 9" '(eyebrowse-switch-to-window-config-9 :which-key "Select Window 9")
 "TAB r" '(eyebrowse-rename-window-config :which-key "Rename Window")
 "TAB n" '(eyebrowse-create-named-window-config :which-key "Create New Window")
 "TAB TAB" '(eyebrowse-switch-to-window-config :which-key "Switch Window")
 "TAB d" '(eyebrowse-close-window-config :which-key "Delete Window")
 "TAB k" '(eyebrowse-next-window-config :which-key "Next Window")
 "TAB j" '(eyebrowse-prev-window-config :which-key "Previous Window"))

(use-package git-gutter-fringe
  :straight (:build t)
  :hook ((prog-mode     . git-gutter-mode)
         (org-mode      . git-gutter-mode)
         (markdown-mode . git-gutter-mode)
         (latex-mode    . git-gutter-mode))
  :diminish
  :config
  (setq git-gutter:update-interval 2)
  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(use-package tsc
  :straight (:build t))
(use-package tree-sitter
  :defer t
  :straight (:build t)
  :init (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :defer t
  :after tree-sitter
  :straight (:build t))

(use-package emacsql-psql
  :defer t
  :after (emacsql)
  :straight (:build t))

(with-eval-after-load 'emacsql
  (dqv/major-leader-key
    :keymaps 'emacs-lisp-mode-map
    :packages '(emacsql)
    "E" #'emacsql-fix-vector-indentation))

(use-package flycheck
  :straight (:build t)
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don’t recheck on idle as often
  (setq flycheck-idle-change-delay 2.0)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to on briefly. This allows “refreshing” the syntax check
  ;; state for several buffers quickly after e.g. changing a config
  ;; file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.2))

(use-package ispell
  :if (executable-find "aspell")
  :defer t
  :straight (:type built-in)
  :config
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (setq ispell-program-name "aspell"
        ispell-extra-args   '("--sug-mode=ultra" "--run-together")
        ispell-aspell-dict-dir (ispell-get-aspell-config-value "dict-dir")
        ispell-aspell-data-dir (ispell-get-aspell-config-value "data-dir")
        ispell-personal-dictionary (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                                                     user-emacs-directory)))

(use-package flyspell
  :defer t
  :straight (:type built-in)
  :ghook 'org-mode 'markdown-mode 'TeX-mode
  :init
  (defhydra flyspell-hydra ()
    "
Spell Commands^^           Add To Dictionary^^              Other
--------------^^---------- -----------------^^------------- -----^^---------------------------
[_b_] check whole buffer   [_B_] add word to dict (buffer)  [_t_] toggle spell check
[_r_] check region         [_G_] add word to dict (global)  [_q_] exit
[_d_] change dictionary    [_S_] add word to dict (session) [_Q_] exit and disable spell check
[_n_] next error
[_c_] correct before point
[_s_] correct at point
"
    ("B" nil)
    ("b" flyspell-buffer)
    ("r" flyspell-region)
    ("d" ispell-change-dictionary)
    ("G" nil)
    ("n" flyspell-goto-next-error)
    ("c" flyspell-correct-wrapper)
    ("Q" flyspell-mode :exit t)
    ("q" nil :exit t)
    ("S" nil)
    ("s" flyspell-correct-at-point)
    ("t" nil))
  :config
  (provide 'ispell) ;; force loading ispell
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :defer t
  :straight (:build t)
  :general ([remap ispell-word] #'flyspell-correct-at-point)
  :config
  (require 'flyspell-correct-ivy nil t))

(use-package flyspell-correct-ivy
  :defer t
  :straight (:build t)
  :after flyspell-correct)

(use-package flyspell-lazy
  :defer t
  :straight (:build t)
  :after flyspell
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(use-package lsp-mode
  :defer t
  :straight (:build t)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode          . lsp-deferred)
         (c++-mode        . lsp-deferred)
         (html-mode       . lsp-deferred)
         (sh-mode         . lsp-deferred)
         (rustic-mode     . lsp-deferred)
         (go-mode         . lsp-deferred)
         ;; (text-mode       . lsp-deferred)
         (move-mode       . lsp-deferred)
         (toml-mode       . lsp-deferred)
         (sql-mode       . lsp-deferred)
         (json-mode       . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (lsp-mode        . lsp-enable-which-key-integration)
         (lsp-mode        . lsp-ui-mode))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-idle-delay 0.6)
  (lsp-use-plist t)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "shellcheck")
                    :major-modes '(sh-mode)
                    :remote? t
                    :server-id 'shellcheck-remote)))

(setq lsp-sqls-workspace-config-path nil)
(setq lsp-enable-indentation nil)
(setq lsp-ui-imenu-auto-refresh nil)
(setq lsp-ui-doc-position 'at-point)

(use-package lsp-ui
  :after lsp
  :defer t
  :straight (:build t)
  :commands lsp-ui-mode
  :config
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(defun dqv/lsp-workspace-remove-missing-projects ()
  (interactive)
  (dolist (dead-project (seq-filter (lambda (x) (not (file-directory-p x))) (lsp-session-folders (lsp-session))))
    (lsp-workspace-folders-remove dead-project)))

(use-package lsp-ivy
  :straight (:build t)
  :defer t
  :after lsp
  :commands lsp-ivy-workspace-symbol)

(defun my-lsp-with-neotree ()
  (interactive)
  (neotree-toggle)
  (lsp))

(use-package exec-path-from-shell
  :defer t
  :straight (:build t)
  :init (exec-path-from-shell-initialize))

(use-package consult-lsp
  :defer t
  :after lsp
  :straight (:build t)
  :general
  (dqv/evil
    :keymaps 'lsp-mode-map
    [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package nginx-mode
  :straight (:build t)
  :defer t)

(use-package company-nginx
  :straight (company-nginx :build t
                           :type git
                           :host github
                           :repo "emacsmirror/company-nginx")
  :defer t
  :config
  (add-hook 'nginx-mode-hook (lambda ()
                               (add-to-list 'company-backends #'company-nginx))))

(use-package csv-mode
  :straight (:build t)
  :defer t
  :general
  (dqv/major-leader-key
    :keymaps 'csv-mode-map
    "a"  #'csv-align-fields
    "d"  #'csv-kill-fields
    "h"  #'csv-header-line
    "i"  #'csv-toggle-invisibility
    "n"  #'csv-forward-field
    "p"  #'csv-backward-field
    "r"  #'csv-reverse-region
    "s"  '(:ignore t :which-key "sort")
    "sf" #'csv-sort-fields
    "sn" #'csv-sort-numeric-fields
    "so" #'csv-toggle-descending
    "t"  #'csv-transpose
    "u"  #'csv-unalign-fields
    "y"  '(:ignore t :which-key yank)
    "yf" #'csv-yank-fields
    "yt" #'csv-yank-as-new-table))

(use-package json-mode
  :straight (:build t)
  :mode "\\.json$"
  :config
  (add-to-list 'flycheck-disabled-checkers 'json-python-json)
  :general
  (dqv/major-leader-key
    :packages 'json-mode
    :keymaps 'json-mode-map
    "f" #'json-pretty-print-buffer))

(defun split-window-right-and-focus ()
  "Spawn a new window right of the current one and focus it."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun split-window-below-and-focus ()
  "Spawn a new window below the current one and focus it."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun kill-buffer-and-delete-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (progn
    (kill-this-buffer)
    (delete-window)))

(use-package eldoc
  ;; :defer t
  :after company
  :preface
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (add-to-list 'display-buffer-alist
            '("^\\*eldoc for" display-buffer-at-bottom
              (window-height . 4)))
  :hook ((eglot-managed-mode . mp-eglot-eldoc))
  :init
  (eldoc-add-command 'company-complete-selection
                     'company-complete-common
                     'company-capf
                     'company-abort)
  :config
  (setq eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-")
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package dockerfile-mode
  :defer t
  :straight (:build t)
  :hook (dockerfile-mode . lsp-deferred)
  :init
  (put 'docker-image-name 'safe-local-variable #'stringp)
  :mode "Dockerfile\\'")

(use-package docker
  :defer t
  :straight (:build t))

(use-package emmet-mode
  :straight (:build t)
  :defer t
  :hook ((css-mode  . emmet-mode)
         (html-mode . emmet-mode)
         (web-mode  . emmet-mode)
         (sass-mode . emmet-mode)
         (scss-mode . emmet-mode)
         (web-mode  . emmet-mode))
  :config
  (general-define-key
   :keymaps 'emmet-mode-keymap
   "M-RET" #'emmet-expand-yas)
  (dqv/major-leader-key
    :keymaps 'web-mode-map
    :packages '(web-mode emmet-mode)
    "e" '(:ignore t :which-key "emmet")
    "ee" #'emmet-expand-line
    "ep" #'emmet-preview
    "eP" #'emmet-preview-mode
    "ew" #'emmet-wrap-with-markup))

(use-package impatient-mode
  :straight (:build t)
  :defer t)

(use-package web-mode
  :defer t
  :straight (:build t)
  :hook html-mode
  :hook (web-mode . prettier-js-mode)
  :hook (web-mode . lsp-deferred)
  :mode (("\\.phtml\\'"      . web-mode)
         ("\\.tpl\\.php\\'"  . web-mode)
         ("\\.twig\\'"       . web-mode)
         ("\\.xml\\'"        . web-mode)
         ("\\.html\\'"       . web-mode)
         ("\\.htm\\'"        . web-mode)
         ("\\.[gj]sp\\'"     . web-mode)
         ("\\.as[cp]x?\\'"   . web-mode)
         ("\\.eex\\'"        . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.hbs\\'"        . web-mode)
         ("\\.eco\\'"        . web-mode)
         ("\\.ejs\\'"        . web-mode)
         ("\\.svelte\\'"     . web-mode)
         ("\\.ctp\\'"        . web-mode)
         ("\\.djhtml\\'"     . web-mode)
         ("\\.vue\\'"        . web-mode))
  :config
  (csetq web-mode-markup-indent-offset 2
         web-mode-code-indent-offset   2
         web-mode-css-indent-offset    2
         web-mode-style-padding        0
         web-mode-script-padding       0)
  :general
  (dqv/major-leader-key
   :keymaps 'web-mode-map
   :packages 'web-mode
   "="  '(:ignore t :which-key "format")
   "E"  '(:ignore t :which-key "errors")
   "El" #'web-mode-dom-errors-show
   "gb" #'web-mode-element-beginning
   "g"  '(:ignore t :which-key "goto")
   "gc" #'web-mode-element-child
   "gp" #'web-mode-element-parent
   "gs" #'web-mode-element-sibling-next
   "h"  '(:ignore t :which-key "dom")
   "hp" #'web-mode-dom-xpath
   "r"  '(:ignore t :which-key "refactor")
   "j"  '(web-mode-tag-match :which-key "Jump Match")
   "rc" #'web-mode-element-clone
   "rd" #'web-mode-element-vanish
   "rk" #'web-mode-element-kill
   "rr" #'web-mode-element-rename
   "rw" #'web-mode-element-wrap
   "z"  #'web-mode-fold-or-unfold)
  (dqv/major-leader-key
    :keymaps 'web-mode-map
    :packages '(lsp-mode web-mode)
    "l" '(:keymap lsp-command-map :which-key "lsp")))

(use-package company-web
  :defer t
  :straight (:build t)
  :after (emmet-mode web-mode))

(use-package css-mode
  :defer t
  :straight (:type built-in)
  :hook (css-mode . smartparens-mode)
  :hook (css-mode . lsp-deferred)
  :hook (scss-mode . prettier-js-mode)
  :init
  (put 'css-indent-offset 'safe-local-variable #'integerp)
  :general
  (dqv/major-leader-key
    :keymaps 'css-mode-map
    :packages 'css-mode
    "=" '(:ignore :which-key "format")
    "g" '(:ignore :which-key "goto")))

(use-package scss-mode
  :straight (:build t)
  :hook (scss-mode . smartparens-mode)
  :hook (scss-mode . lsp-deferred)
  :hook (scss-mode . prettier-js-mode)
  :defer t
  :mode "\\.scss\\'")

(use-package counsel-css
  :straight (:build t)
  :defer t
  :init
  (cl-loop for (mode-map . mode-hook) in '((css-mode-map  . css-mode-hook)
                                           (scss-mode-map . scss-mode-hook))
           do (add-hook mode-hook #'counsel-css-imenu-setup)
           (dqv/major-leader-key
            :keymaps mode-map
            "gh" #'counsel-css)))

(use-package less-css-mode
  :straight  (:type built-in)
  :defer t
  :mode "\\.less\\'"
  :hook (less-css-mode . smartparens-mode)
  :hook (less-css-mode . lsp-deferred)
  :hook (less-css-mode . prettier-js-mode))

(use-package rjsx-mode
  :defer t
  :straight (:build t)
  :after compile
  :mode "\\.[mc]?jsx?\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"
  :interpreter "node"
  :hook (rjsx-mode . rainbow-delimiters-mode)
  :hook (rjsx-mode . lsp-deferred)
  :hook (rjsx-mode . prettier-js-mode)
  :init
  (add-to-list 'compilation-error-regexp-alist 'node)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
                      2 3 4))
  :general
  (dqv/major-leader-key
    :keymaps 'rjsx-mode-map
    "rr" #'rjsx-rename-tag-at-point
    "rj" #'rjsx-jump-tag)
  (dqv/evil
    :keymaps 'rjsx-mode-map
    "s-;" #'rjsx-jump-tag
    "s-r" #'rjsx-rename-tag-at-point)
  :config
  (setq js-chain-indent                  t
        js2-basic-offset                 2
        ;; ignore shebangs
        js2-skip-preprocessor-directives t
        ;; Flycheck handles this already
        js2-mode-show-parse-errors       nil
        js2-mode-show-strict-warnings    nil
        ;; conflicting with eslint, Flycheck already handles this
        js2-strict-missing-semi-warning  nil
        js2-highlight-level              3
        js2-idle-timer-delay             0.15))

(use-package js2-refactor
  :defer t
  :straight (:build t)
  :after (js2-mode rjsx-mode)
  :hook (js2-mode . js2-refactor-mode)
  :hook (rjsx-mode . js2-refactor-mode))

(use-package npm-transient
  :defer t
  :straight (npm-transient :build t
                           :type git
                           :host github
                           :repo "Phundrak/npm-transient"))

(use-package prettier-js
  :defer t
  :straight (:build t)
  :after (rjsx-mode web-mode typescript-mode)
  :hook (rjsx-mode . prettier-js-mode)
  :hook (js-mode . prettier-js-mode)
  :hook (typescript-mode . prettier-js-mode)
  :config
  (setq prettier-js-args '("--trailing-comma" "all" "--bracket-spacing" "true")))

(use-package typescript-mode
  :defer t
  :straight (:build t)
  :hook (typescript-mode     . rainbow-delimiters-mode)
  :hook (typescript-mode     . lsp-deferred)
  :hook (typescript-mode     . prettier-js-mode)
  :hook (typescript-tsx-mode . rainbow-delimiters-mode)
  :hook (typescript-tsx-mode . lsp-deferred)
  :hook (typescript-tsx-mode . prettier-js-mode)
  :hook (typescript-tsx-mode . eglot-ensure)
  :commands typescript-tsx-mode
  :after flycheck
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  :general
  (dqv/major-leader-key
    :packages 'lsp
    :keymaps '(typescript-mode-map typescript-tsx-mode-map)
    :infix "a"
    ""  '(:keymap lsp-command-map :which-key "lsp")
    "=" '(:ignore t :which-key "format")
    "a" '(:ignore t :which-key "actions"))
  (dqv/major-leader-key
    :packages 'typescript-mode
    :keymaps '(typescript-mode-map typescript-tsx-mode-map)
    "n" '(:keymap npm-mode-command-keymap :which-key "pnpm"))
  :config
  (setq typescript-indent-level 2)
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
    (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))
  (when (fboundp 'web-mode)
    (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX"))
  (autoload 'js2-line-break "js2-mode" nil t))

(use-package tide
  :defer t
  :straight (:build t)
  :hook (tide-mode . tide-hl-identifier-mode)
  :config
  (setq tide-completion-detailed              t
        tide-always-show-documentation        t
        tide-server-may-response-length       524288
        tide-completion-setup-company-backend nil)

  (advice-add #'tide-setup :after #'eldoc-mode)

  :general
  (dqv/major-leader-key
    :keymaps 'tide-mode-map
    "R"   #'tide-restart-server
    "f"   #'tide-format
    "rrs" #'tide-rename-symbol
    "roi" #'tide-organize-imports))

(use-package go-mode
  :straight (:build t)
  :defer t
  :mode ("\\.go\\'" . go-mode))

;; (lsp-register-custom-settings
;;  '(("gopls.completeUnimported" t t)
;;    ("gopls.staticcheck" t t)))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(add-hook 'go-mode-hook 'eglot-ensure)

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

(use-package yaml-mode
  :ensure t)



(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(use-package protobuf-mode
  :mode "\\.proto3")
