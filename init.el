;; use package.el to install packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(eval-when-compile (require 'cl))

(defvar my-packages
  '(
;; faces
    color-theme color-theme-molokai
    nyan-mode
    smart-mode-line

;; behaviors
    smooth-scrolling
    expand-region
    idle-highlight
    iedit
    undo-tree
    icicles lacarte
    multiple-cursors
    ido-vertical-mode ido-hacks ido-better-flex ido-ubiquitous smex
    flx-ido flex-isearch
    goto-last-change

;; utilities
    dired+
    gist
    powershell
    xkcd
    magit magit-push-remote
    ace-jump-mode
    projectile

;; global text exts
    paredit
    rainbow-mode
    volatile-highlights
    pretty-mode
    smartparens
    flycheck flycheck-haskell
    yasnippet
    god-mode

;; languages
    auctex
    coffee-mode
    enh-ruby-mode inf-ruby
    markdown-mode
    python
    scala-mode2
    haskell-mode shm ; Structured-Haskell-Mode
    org
)
  "A list of packages to ensure are installed at launch.")

(package-initialize)
;; check if the packages is installed; if not, install it.
(let ((xs (remove-if 'package-installed-p my-packages)))
  (and xs
    (progn
      (package-refresh-contents)
      (mapc 'package-install xs))))
;; end package.el



;; Settings

; Garbage collect per 200MB allocaled; for flx
(setq gc-cons-threshold 200000000)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

; (setq file-name-coding-system 'japanese-shift-jis-dos)

(setq user-full-name "LauZi")
(setq user-mail-address "st61112@gmail.com")

(setq load-path (cons (expand-file-name "~/.emacs.d/lisp/") load-path))

;; Disable input methods
(global-unset-key (kbd "C-\\"))

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-\\") 'er/expand-region)
(global-set-key (kbd "C-|") 'er/contract-region)

(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-.") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-c C-,") 'mc/mark-all-words-like-this)


;; key bindings to adjust frame size
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; match-paren defined below
(global-set-key (kbd "C-%") 'match-paren)

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(global-set-key (kbd "M-k") 'qiang-copy-line)

(global-set-key (kbd "<f1>") 'ansi-term)

(global-set-key [(control ?\')] 'other-window)  ;; C-'

(global-set-key [?\e ?\M-x] 'lacarte-execute-command)  ;; ESC M-x
(global-set-key [?\M-`] 'lacarte-execute-command)      ;; M-`

(global-set-key (kbd "C-x C-/") 'goto-last-change)

(global-set-key (kbd "C-z") 'god-mode-all)  ; 'god-local-mode

;;; highlight ()
(color-theme-molokai)
(set-face-attribute 'default nil :font "Consolas-12")
(setq frame-title-format "Emacs 24 @ %b")  ;; show buffername in title


(nyan-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(show-paren-mode 1)
(setq tab-width 4)


(global-linum-mode t)

(setq column-number-mode t)
(setq size-indication-mode t)


(mouse-avoidance-mode 'animate)

(icy-mode 1)


(global-auto-revert-mode t) ;; auto-refresh files

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq make-backup-files nil)

;; FUCK VC-MODE
(setq vc-handled-backends ())
;;; end Settings


;; Addons
(defadvice comment-or-uncomment-region (before slickcomment activate compile)
  "When called interactively with no active region, toggle comment on current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;;C mode
(defun my-c-mode-hook ()
               (c-set-style "cc-mode")

               (setq c-indent-level 4)
               (setq c-tab-width 4)
	       (setq tab-width 4)
               (setq c-basic-offset tab-width)
               (setq indent-tabs-mode nil) ;; force only spaces for indentation
	       (setq abbrev-mode nil)
	       (define-key c-mode-base-map [(return)] 'newline-and-indent)
	       (define-key c-mode-base-map [(f9)] 'compile)
	       (define-key c-mode-base-map [(meta \')] 'c-indent-command)
	       (define-key c-mode-base-map "\M-;" 'comment-or-uncomment-region)
           )
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
(setq kill-whole-line t) ; makes kill-line remove whole line


;start smart-beginning-of-line
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
;end smart-beginning-of-line


;start qiang-copy-line
(defun qiang-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (point)
                  (line-end-position))

  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
;end qiang-copy-line


; begin auto-insert for .sh
(define-auto-insert 'sh-mode '(nil "#!/bin/bash\n\n"))
(define-auto-insert 'ptyhon-mode '(nil "#!/usr/bin/python\n\n"))
(add-hook 'find-file-hooks 'auto-insert)


;; AUCTeX settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook (lambda()
			     (setq TeX-save-query  nil
				   TeX-engine 'xetex
				   TeX-show-compilation t)
			     (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
			     ))


;; removes buffer after ansi-term closes
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

;; makes URLs clickable
(defun my-term-hook ()
  (goto-address-mode))
(add-hook 'term-mode-hook 'my-term-hook)


;; detect octave files
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))


;; change backup files(*~) and autosave files (#*#) directory
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; Jump to corresponding paren
;; http://docs.huihoo.com/homepage/shredderyin/emacs_elisp.html

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(add-hook 'haskell-mode-hook 'structured-haskell-mode)

(setq haskell-font-lock-symbols t)

(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "<f9>") 'haskell-compile))

(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "<f9>") 'haskell-compile))


;; uniquify buffer names
(setq uniquify-buffer-name-style 'post-forward)  ; must set, or uniquify will not work
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
; (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; tab now omit those extensions
(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      '(".o" ".lo" ".mh" ".elc" "~" ".bak" ".err"
		".bin" ".lbin" ".fasl"
		".dvi" ".toc" ".aux" ".lof" ".blg" ".bbl"
		".glo" ".idx" ".lot" ".dvi" ".ps"
		".hi"
		".cache/" ".dropbox/" ".git/"
		".BIN/" "System Volume Information/")))  ;; trailing / means directory


;; Dired+
(define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file)

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-omit-mode 1)
	    ))

;; do not list those shit in dired+
(setq dired-omit-files
      "^\\.?#\\|^\\.$^\\.?#\\|^\\.$\\|$RECYCLE\\.BIN\\|System Volume Information\\|\\.dropbox.*\\|.git\\|.*\\.hi")

; (add-hook 'prog-mode-hook (lambda () (idle-highlight t)))

(global-undo-tree-mode)

; ruby
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)


; smartparens
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

(sp-local-pair 'haskell-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

; end smartparens

; smart-mode-line
(sml/setup)

(setq sml/name-width 30)
(setq sml/mode-width 'full)
(setq sml/shorten-directory nil)
(setq sml/shorten-modes nil)

(add-to-list 'sml/replacer-regexp-list '("^:Scala:gameData/src/main/scala/" ":gameData:"))
(add-to-list 'sml/replacer-regexp-list '("^:DB:scala/" ":Scala:"))
(add-to-list 'sml/replacer-regexp-list '("^D:/Dropbox/" ":DB:"))

(add-to-list 'sml/hidden-modes " SP")
(add-to-list 'sml/hidden-modes " Undo-Tree")
(add-to-list 'sml/hidden-modes " MRev")
(add-to-list 'sml/hidden-modes " Ind")
(add-to-list 'sml/hidden-modes " Doc")
(add-to-list 'sml/hidden-modes " yas")
(add-to-list 'sml/hidden-modes " SHM")

;; ido-mode
; sort ido filelist by mtime instead of alphabetically
;  (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
;  (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
  (defun ido-sort-mtime ()
    (setq ido-temp-list
          (sort ido-temp-list
                (lambda (a b)
                  (time-less-p
                   (sixth (file-attributes (concat ido-current-directory b)))
                   (sixth (file-attributes (concat ido-current-directory a)))))))
    (ido-to-end  ;; move . files to end (again)
     (delq nil (mapcar
                (lambda (x) (and (char-equal (string-to-char x) ?.) x))
                ido-temp-list))))

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(ido-ubiquitous)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;If don't want to use the flx's highlights you can turn them off like this:
(setq flx-ido-use-faces nil)
;(ido-vertical-mode t)

(flex-isearch-mode t)

;; smex
(smex-initialize) ; preload on setup

(global-set-key (kbd "M-x") 'smex)

; Commands related to current major mode
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;


;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  (lambda ()
    (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)))
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))

(yas-global-mode t)


;; org-mode
(setq org-pretty-entities t)
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "C-\'") nil)))
;; end org-mode

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; projectile
(setq projectile-indexing-method 'native) ; only needed on windows
(projectile-global-mode)

;; projectile

;; God-mode
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

;(define-key god-local-mode-map (kbd "i") 'god-local-mode)

;; end god-mode

(load "~/.emacs.d/local")

(setq debug-on-error t)
