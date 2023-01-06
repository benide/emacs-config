;;; init-projects.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(elpaca-use-package projectile :demand t
  :custom
  (projectile-project-search-path '("/home/ben/Projects/"))
  :config
  (projectile-mode 1))

(defun my/project-find (dir)
  (if-let ((root (locate-dominating-file dir ".project")))
      (cons 'local root)))
(defun my/projectile-find (dir)
  (if-let ((root (locate-dominating-file dir ".projectile")))
      (cons 'local root)))

(elpaca nil ;; project.el stuff

  ;; remove the `my/project-find' and `cl-defmethod' if we use project-x
  (cl-defmethod project-root ((project (head local))) (cdr project))
  (add-hook 'project-find-functions #'my/project-find)

  ;; this requires either the above `cl-defmethod' or `project-x'
  (add-hook 'project-find-functions #'my/projectile-find))

;; (use-package counsel-projectile
;;   :after counsel
;;   :demand t
;;   :config
;;   (counsel-projectile-mode))


(provide 'init-projects)

;;; init-projects.el ends here
