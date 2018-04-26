;;; private/cjg/config.el -*- lexical-binding: t; -*-

(if (featurep! +bindings) (load! +bindings))

(def-package! gruvbox)

(setq doom-font (font-spec :family "Input Mono Narrow" :weight 'light :size 13))
(setq doom-theme 'gruvbox)
(setq mac-command-modifier 'alt
      mac-option-modifier  'meta)

(setq-default line-spacing 0.4)
(global-visual-line-mode t)

(after! js2-mode
  (setq-default js2-highlight-level 3)
  (setq-default js2-include-browser-externs +1)
  (setq-default js2-include-node-externs +1))


;; By setting company-idle-delay to a non-nil float, it enables company mode everywhere
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)

  ;; Always include company-files as a backend to autocomplete potential file system references.
  (push 'company-files company-backends))

(defun cjg/add-js-prettify-symbols()
  (push '("function" . ?ƒ) prettify-symbols-alist)
  (push '("require" . ?ի) prettify-symbols-alist)
  (prettify-symbols-mode))

;; TODO
;; 1. integrate flow into javascript linting
;; 2. Figure out automatic file path look up for js / less files.
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'js2-imenu-extras)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook 'cjg/add-js-prettify-symbols)
(add-hook 'rjsx-mode-hook 'js2-imenu-extras-mode)
(add-hook 'rjsx-mode-hook 'cjg/add-js-prettify-symbols)

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

(global-set-key (kbd "C-M-=") 'increase-default-font-height)
(global-set-key (kbd "C-M--") 'decrease-default-font-height)
