;;; private/cjg/+bindings.el -*- lexical-binding: t; -*-
(defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

;; Example 1
(defun counsel-rg-thing-at-point ()
  (interactive)
  ;; TODO: fix this call to work with counsel-rg
  (ivy-with-thing-at-point 'counsel-ag))

;; Example 2
(defun swiper-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'swiper))

(map!
 ;; --- <leader> -------------------------------------
 (:leader
   (:desc "Search for Word" :nv "*" #'counsel-rg-thing-at-point)
   (:desc "resume" :prefix "l"
     :desc "Ivy Resume"              :nv "r"  #'ivy-resume
     )))

