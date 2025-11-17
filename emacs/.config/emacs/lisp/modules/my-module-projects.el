;;; my-module-projects.el --- Project/navigation tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Projectile for project management and Magit for git workflows.

;;; Code:

(use-package projectile
  :straight t
  :init
  (setq projectile-indexing-method 'alien
        projectile-use-git-grep t)
  :config
  (projectile-mode +1))

(use-package magit
  :straight t)

(provide 'my-module-projects)

;;; my-module-projects.el ends here
