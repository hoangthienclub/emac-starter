;; NOTE: Setting `font-lock-multiline' to 'undecided' org-mode to become unusable:
;; Error during redisplay: (jit-lock-function 22138) signaled (args-out-of-range 0 16341)
(setq-default font-lock-multiline nil)

;;; Better defaults
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; better simple ui
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(push '(ns-use-native-fullscreen . t) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)


(setq visible-bell nil ;; set to non-nil to flash!
      ring-bell-function 'ignore
      large-file-warning-threshold (* 50 1024 1024) ;; change to 50 MiB
      use-short-answers t ;; y or n istead of yes or no
      confirm-kill-emacs 'y-or-n-p ;; confirm before quitting
      inhibit-startup-message t
      delete-by-moving-to-trash t)

(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-speed 3)
(customize-set-variable 'native-comp-deferred-compilation nil)

 (setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      frame-title-format nil
      truncate-lines t
      truncate-partial-width-windows t
      package-enable-at-startup nil
      indicate-buffer-boundaries '((bottom . right))
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-compacting-font-caches t
      initial-scratch-message nil
      load-prefer-newer noninteractive
      site-run-file nil)

;; (setq debug-on-error t)

;;; Undo
(setq undo-limit        10000000 ;; 1MB (default is 160kB)
      undo-strong-limit 100000000 ;; 100MB (default is 240kB)
      undo-outer-limit  1000000000) ;; 1GB (default is 24MB)

;;; Editing
(setq display-line-numbers-type 'relative
      tab-always-indent nil
      whitespace-action '(cleanup auto-cleanup))

(setq-default fill-column 140
              indent-tabs-mode nil
              display-line-numbers-width 3
              tab-width 2)

;;; Backups
;; Disable backup and lockfiles
(setq create-lockfiles nil
      make-backup-files nil
      version-control t ;; number each backup file
      backup-by-copying t ;; copy instead of renaming current file
      delete-old-versions t ;; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      tramp-backup-directory-alist backup-directory-alist)

;;; Auto-Saving, sessions...
;; Enable auto-save (use `recover-file' or `recover-session' to recover)
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;;; Scrolling
(setq hscroll-step 2
      hscroll-margin 5
      scroll-step 2
      scroll-margin 5
      scroll-conservatively 101
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

(setq window-combination-resize t)

(setq recentf-max-saved-items 100)

;;; Enable global modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Scroll pixel by pixel
(pixel-scroll-mode 1)

;; Window layout undo/redo (`winner-undo' / `winner-redo')
(winner-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Enable recentf-mode globally
(recentf-mode 1)

;; Save place in files
(save-place-mode 1)

;; Enable saving minibuffer history
(savehist-mode 1)

;; Show line and column numbers (cursor position) in mode-line
(line-number-mode 1)

;; Wrap long lines
(global-visual-line-mode 1)

;; Better handling for files with so long lines
(global-so-long-mode 1)

;; Global SubWord mode
(global-subword-mode 1)

;; Set Fullscreen
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ;; Set Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (100 . 100)))
