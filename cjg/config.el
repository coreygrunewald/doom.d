;;; private/cjg/config.el -*- lexical-binding: t; -*-

(if (featurep! +bindings) (load! +bindings))

(def-package! gruvbox)
(def-package! company-flow)
(def-package! company-tern)
(def-package! flycheck-flow)
(def-package! ocp-indent)

;; TODO: enhance company flow predicate to only check files with // @flow at the top
(setq doom-font (font-spec :family "Input Mono Narrow" :weight 'light :size 13))
(setq doom-theme 'gruvbox)
(setq mac-command-modifier 'alt
      mac-option-modifier  'meta)
(setq-default line-spacing 0.4)

(global-visual-line-mode t)

(defun cjg-javascript-flow-predicate()
  (and
   buffer-file-name
   (file-exists-p buffer-file-name)
   (cjg-check-for-buffer-text
    (buffer-substring-no-properties (point-min) (point-max))
    "@flow")
   (locate-dominating-file buffer-file-name ".flowconfig")))

;; By setting company-idle-delay to a non-nil float, it enables company mode everywhere
(after! company
  ;;Always include company-files as a backend to autocomplete potential file system references.
  (push 'company-files company-backends)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))

(defun cjg/add-js-prettify-symbols()
  (push '("function" . ?ƒ) prettify-symbols-alist)
  (push '("require" . ?ի) prettify-symbols-alist)
  (prettify-symbols-mode))

;; TODO
;; 1. integrate flow into javascript linting
;; 2. Figure out automatic file path look up for js / less files.
(add-hook 'before-save-hook 'whitespace-cleanup)

(set! :company-backend 'js2-mode '(company-flow company-tern))
(set! :company-backend 'rjsx-mode '(company-flow company-tern))

(defun cjg-use-flow-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (global-flow (executable-find "flow"))
         (local-flow (expand-file-name "node_modules/.bin/flow"
                                       root))
         (flow (if (file-executable-p local-flow)
                   local-flow
                 global-flow)))
    (setq-local company-flow-executable flow)
    (setq-local flycheck-javascript-flow-executable flow)
    (setq-local flycheck-javascript-flow-coverage-executable flow)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow-coverage)
    ))

(after! js2-mode tern-mode
  (require 'js2-imenu-extras)
  (setq-default js2-highlight-level 3)
  (setq-default js2-include-browser-externs +1)
  (setq-default js2-include-node-externs +1)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook 'tern-mode)
  (add-hook 'js2-mode-hook 'cjg/add-js-prettify-symbols)
  (add-hook 'js2-mode-hook 'cjg-use-flow-from-node-modules)
  (add-hook 'rjsx-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'rjsx-mode-hook 'tern-mode)
  (add-hook 'rjsx-mode-hook 'cjg/add-js-prettify-symbols)
  (add-hook 'rjsx-mode-hook 'cjg-use-flow-from-node-modules))

;; NOTE: This is needed for Emacs 26
(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)))

(defun font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
    The pixel size of the frame is kept (approximately) the same.
    DELTA should be a multiple of 10, in the units used by the
    :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (font-name-replace-size (face-font 'default)
                                                new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun increase-default-font-height ()
  (interactive)
  (increment-default-font-height 10))

(defun decrease-default-font-height ()
  (interactive)
  (increment-default-font-height -10))

(after! magit
 (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(setq shell-command-default-error-buffer "*Messages*")

;; NOTE: this is need b/c zsh is breaking some command outputs
;; (setq shell-file-name "/bin/bash")

(after! projectile
  ;; TODO: projectile caching broken b/c of incomplete shell command output (truncated)
  ;; try upgrading to newer emacs to see if it is fixed?
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root t))

(defun cjg/init-ocaml-opam ()
  (if (executable-find "opam")
      (let* ((output (shell-command-to-string
                      "opam config var share 2> /dev/null"))
             (share (when (< 0 (length output))
                      (substring output 0 -1))))
          (when share
            (setq opam-share share
                  opam-load-path (concat share "/emacs/site-lisp")))
          (add-to-list 'load-path opam-load-path))
    (print "Can't find opam executable")))

(after! tuareg
  (cjg/init-ocaml-opam)
  (setq tuareg-electric-indent nil)
  (setq opam (concat (substring (shell-command-to-string "opam config var prefix 2> /dev/null") 0 -1) "/"))
  (setq tuareg-library-path opam))

(after! evil-goggles
  (evil-goggles-use-diff-faces))

(defun cjg/advise-shell-command (orig-fun &rest args)
  (let (current-shell (shell-file-name))
    (setq shell-file-name "/bin/bash")
    (let ((res (apply orig-fun args)))
      (setq shell-file-name current-shell)
      res)))

(advice-add 'projectile-files-via-ext-command :around #'cjg/advise-shell-command)
(advice-add 'merlin-command :around #'cjg/advise-shell-command)
(advice-add 'merlin--call-process :around #'cjg/advise-shell-command)
;;(advice-add 'tuareg-shell-command-to-string :around #'cjg/advise-shell-command)

(global-set-key (kbd "C-M-=") 'increase-default-font-height)
(global-set-key (kbd "C-M--") 'decrease-default-font-height)
