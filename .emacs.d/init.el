;; Packages

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp clojure-mode nrepl ac-nrepl clojure-project-mode
                                  clojure-test-mode rainbow-delimiters)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'clojure-project-mode)

;; org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/.emacs.d/org/todo.org"))

(menu-bar-mode 1)

(set-face-attribute 'default nil :font "Monaco-10")

(setq shift-select-mode t)
(global-auto-revert-mode t)

;; This is nice to enable for modes which don't already have it:
(global-set-key "\C-m" 'newline-and-indent)

;; http://emacs.wordpress.com/2007/01/22/killing-yanking-and-copying-lines/
(setq kill-whole-line t)
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
                                           c-mode c++-mode objc-mode clojure-mode
                                           LaTeX-mode TeX-mode))
      (indent-region (region-beginning) (region-end) nil)))


(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; (setq auto-save-file-name-transforms
;;       `((".*" ,(expand-file-name "~/.emacs.d/auto-saves/"))))

;; (setq auto-save-file-name-transforms
;;       `(("#.*#" ,"~/.emacs.d/auto-saves/" t)))

(defvar autosave-dir (concat "~/.emacs.d/auto-saves" "/"))
(make-directory autosave-dir t)
(setq auto-save-file-name-transforms
      `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat autosave-dir "\\1") t)))

;; (defun auto-save-file-name-p (filename)
;;   (string-match "^#.*#$" (file-name-nondirectory filename)))
;; (defun make-auto-save-file-name ()
;;   (concat autosave-dir
;;           (if buffer-file-name
;;               (concat "#" (file-name-nondirectory buffer-file-name) "#")
;;             (expand-file-name
;;              (concat "#%" (buffer-name) "#")))))


;; http://www.emacswiki.org/emacs/Scrolling
;; (global-set-key [next]
;;                 (lambda () (interactive)
;;                   (condition-case nil (scroll-up)
;;                     (end-of-buffer (goto-char (point-max))))))

;; (global-set-key [prior]
;;                 (lambda () (interactive)
;;                   (condition-case nil (scroll-down)
;;                     (beginning-of-buffer (goto-char (point-min))))))

(defun sfp-page-down (&optional arg)
  (interactive "^P")
  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))
(put 'sfp-page-down 'isearch-scroll t)
;; (put 'sfp-page-down 'CUA 'move)

(defun sfp-page-up (&optional arg)
  (interactive "^P")
  (setq this-command 'previous-line)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines)))
(put 'sfp-page-up 'isearch-scroll t)
;; (put 'sfp-page-up 'CUA 'move)

(global-set-key [next] 'sfp-page-down)
(global-set-key [prior] 'sfp-page-up)

;; http://geosoft.no/development/emacs.html
(global-set-key [kp-home]  'beginning-of-buffer) ; [Home]
(global-set-key [home]     'beginning-of-buffer) ; [Home]
(global-set-key [kp-end]   'end-of-buffer)       ; [End]
(global-set-key [end]      'end-of-buffer)       ; [End]

(global-set-key (kbd "C-z") 'undo) ; [Undo]. Ctrl-Z is otherwise bound
                                        ; to hide the current frame, which
                                        ; is pretty useless and easy to
                                        ; trigger by accident

;; nrepl config

(require 'nrepl)
(require 'ac-nrepl)
;; workaround for https://github.com/kingtim/nrepl.el/issues/168
(setq nrepl-connected-hook (reverse nrepl-connected-hook))

;; Allow right alt key to be used for alt on OS X -- e.g.
;; to enter special characters -- rather than act as META.
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

;; nrepl's bindings for these clash with paredit:
(define-key nrepl-mode-map (kbd "M-P") 'nrepl-previous-matching-input)
(define-key nrepl-mode-map (kbd "M-N") 'nrepl-next-matching-input)

(require 'rainbow-delimiters)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)


;; Stop the error buffer from popping up while working in the REPL buffer:
(setq nrepl-popup-stacktraces nil)

;; Make C-c C-z switch to the *nrepl* buffer in the current window:
(add-to-list 'same-window-buffer-names "*nrepl*")

;; auto-complete and ac-nrepl config

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-mode-hook 'auto-complete-mode)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'auto-complete-mode)

(eval-after-load "auto-complete"
  '(progn (add-to-list 'ac-modes 'nrepl-mode 'nrepl-interaction-mode)
          ;; http://cx4a.org/software/auto-complete/manual.html
          (setq ac-auto-start t)
          (setq ac-auto-show-menu t)))

;; this turns off emacs annoying 'completion list' buffer
(setq completion-auto-help nil)

;; Not really sure what this added:
;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)


;;(global-set-key [delete] 'my-delete)

(require 'paredit)

(defun paredit-backward-maybe-delete-region ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'kill-region)
    (paredit-backward-delete)))

(defun paredit-forward-maybe-delete-region ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'kill-region)
    (paredit-forward-delete)))

(defun paredit-copy-as-kill-region-or-sexp ()
  (interactive)
  (if mark-active
      (call-interactively 'copy-region-as-kill)
    (call-interactively 'paredit-copy-as-kill)))

;; include various punctuation used in clojure symbols & keywords, when skipping words:
;; http://stackoverflow.com/questions/7544536/arbitrary-characters-for-forward-word-backward-word-in-emacs
(modify-syntax-entry ?- "w" clojure-mode-syntax-table)
(modify-syntax-entry ?: "w" clojure-mode-syntax-table)
(modify-syntax-entry ?* "w" clojure-mode-syntax-table)
(modify-syntax-entry ?> "w" clojure-mode-syntax-table)
(modify-syntax-entry ?< "w" clojure-mode-syntax-table)

;; todo: wrap square/curly variants too
(defun paredit-wrap-sexp-maybe-from-middle ()
  "By default paredit-wrap-sexp will split a keyword in the middle if point is in middle,
   not very useful. This one skips to start of keyword first, then wraps, then skips to end
   ready to type further stuff."
  (interactive)
  (forward-char 1)
  (backward-word 1)
  (call-interactively 'paredit-wrap-sexp)
  (forward-word 1))

(define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-sexp-maybe-from-middle)
;; useful for clojure
(define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
(define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
;; make M-x copy sexps as C-k kills sexps in paredit mode, except
;; when region active in which case copy region:
(define-key paredit-mode-map (kbd "M-w") 'paredit-copy-as-kill-region-or-sexp)
;; delete region via del/backspc to work in paredit
(define-key paredit-mode-map (kbd "<delete>") 'paredit-forward-maybe-delete-region)
(define-key paredit-mode-map (kbd "DEL") 'paredit-backward-maybe-delete-region)
;; forward delete
(define-key paredit-mode-map (kbd "M-S-<delete>") 'paredit-forward-kill-word)

;; Some clojure-mode extras
;; indent some of my fn-style macros properly:
(put-clojure-indent 'score-fn 'defun)
(put-clojure-indent 'scoring-model 'defun)

;; nrepl helper -- clears all non-core bindings from a namespace.
;; useful prior to C-c C-k if new ns form clashes with old
(defun nrepl-clear-ns (ns)
  "Clear the namespace of the current clojure buffer of all maps and aliases"
  (interactive (list (nrepl-find-ns)))
  (with-current-buffer nrepl-nrepl-buffer
    (nrepl-send-string
     (format "((fn clear [ns]
  (doseq [[sym _] (ns-aliases ns)]
         (ns-unalias ns sym))
  (doseq [[sym v] (ns-refers ns)]
         (when-not (.startsWith (str (.ns v)) \"clojure\")
                   (ns-unmap ns sym)))) (find-ns '%s))" ns)
     (nrepl-handler (current-buffer)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; http://stackoverflow.com/questions/1884301/prevent-emacs-switch-to-buffer-other-window-from-resizing-other-window
(setq even-window-heights nil)


;; based on emacs-starter-kit starter-bindings

;; It's all about the project.
(global-set-key (kbd "C-c f") 'find-file-in-project)

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Window switching. (C-x o goes to the next window)
;; (windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "M-s-<left>")  'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)
(global-set-key (kbd "M-s-<up>")    'windmove-up)
(global-set-key (kbd "M-s-<down>")  'windmove-down)

(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x C-m") 'shell)

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-c x") 'execute-extended-command)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'esk-eval-and-replace)

;; M-S-6 is awkward
(global-set-key (kbd "C-c q") 'join-line)

;; So good!
(global-set-key (kbd "C-c g") 'magit-status)

;; This is a little hacky since VC doesn't support git add internally
(eval-after-load 'vc
  (define-key vc-prefix-map "i"
    '(lambda () (interactive)
       (if (not (eq 'Git (vc-backend buffer-file-name)))
           (vc-register)
         (shell-command (format "git add %s" buffer-file-name))
         (message "Staged changes.")))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))


;;; Fix paredit for clojure


;;; auctex

(setq TeX-PDF-mode t)

(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince -f %o")
        ("^html?$" "." "iceweasel %o"))))

;; file management

;; from http://tuxicity.se/emacs/elisp/2010/03/26/rename-file-and-buffer-in-emacs.html
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name     (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;; align

;;doesn't seem to work (maybe for older emacs version?):
;;(add-to-list 'align-lisp-modes 'clojure-mode)

;; Doesn't seem to do what I want:
;; (add-to-list 'align-rules-list
;;              '(clojure-keyword-map
;;                (regexp . ":[^\s]+\\(\s+\\).+")
;;                (group  . 1)
;;                (modes  . align-lisp-modes)))

;; whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; speedbar
;; (speedbar 1)

;; translucent bg from http://emacs-fu.blogspot.co.uk/2009/02/transparent-emacs.html
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

;; C-8 will increase opacity (== decrease transparency)
;; C-9 will decrease opacity (== increase transparency
;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;; Helps with LaTeX etc
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Java

;; java-mode is defined here:
(require 'cc-mode)

;; sudo apt-get install openjdk-6-source


(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode nil)))

;; Have this work as in clojure mode
(define-key java-mode-map (kbd "M-q") 'c-indent-defun)

;; Start
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
;; to allow $EDITOR to open file in existing emacs process.
;;
;; Use M-x # to finish editing a buffer and return it to emacsclient
(server-start)
