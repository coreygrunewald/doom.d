;;; private/cjg/+bindings.el -*- lexical-binding: t; -*-
(defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

;; Example 1
(defun counsel-ag-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'counsel-ag))

;; Example 2
(defun swiper-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'swiper))

(map!
 ;; --- <leader> -------------------------------------
 (:leader
   (:desc "Search for Word" :nv "*" #'counsel-ag-thing-at-point)
   (:desc "Swiper for Word" :nv "#" #'swiper-thing-at-point)
   (:desc "Tern Definition" :n "ct" #'tern-find-definition)
   (:desc "resume" :prefix "l"
     :desc "Ivy Resume"              :nv "r"  #'ivy-resume
     )))

