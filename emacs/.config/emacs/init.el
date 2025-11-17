;; Paths and loader scaffolding
(defconst my/emacs-config-root
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of my Emacs configuration.")

(defconst my/emacs-lisp-dir
  (expand-file-name "lisp" my/emacs-config-root)
  "Directory containing modular Emacs Lisp files.")

(defconst my/emacs-var-dir
  (expand-file-name "var" my/emacs-config-root)
  "Directory for generated state (custom.el, cache, etc.).")

(defconst my/emacs-vendor-dir
  (expand-file-name "../../lisp" my/emacs-config-root)
  "Directory with local vendor packages.")

(defun my/add-to-load-path (path)
  "Add PATH to `load-path' when PATH exists."
  (when (file-directory-p path)
    (add-to-list 'load-path path)))

(my/add-to-load-path my/emacs-lisp-dir)
(dolist (subdir '("core" "modules" "lang"))
  (my/add-to-load-path (expand-file-name subdir my/emacs-lisp-dir)))
(my/add-to-load-path my/emacs-vendor-dir)

(unless (file-directory-p my/emacs-var-dir)
  (make-directory my/emacs-var-dir t))

(setq custom-file (expand-file-name "custom.el" my/emacs-var-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(defun my/load-module (feature)
  "Require FEATURE safely and log failures."
  (condition-case err
      (progn
        (require feature)
        (message "[init] Loaded %s" feature))
    (error
     (display-warning 'init
                      (format "Failed to load %s: %s" feature err)
                      :error))))

(defun my/load-modules (modules)
  "Load each feature from MODULES sequentially."
  (dolist (feature modules)
    (my/load-module feature)))

(defvar my/emacs-modules
  '(my-core-settings
    my-core-packages
    my-core-editing
    my-module-completion
    my-module-evil
    my-module-ui
    my-module-org
    my-module-org-roam
    my-module-projects
    my-module-notes
    my-module-navigation
    my-lang-lsp
    my-lang-c
    my-lang-web
    my-lang-ocaml
    my-lang-python
    my-lang-tex
    my-core-keybinds)
  "Ordered list of modules to load at startup.")

(my/load-modules my/emacs-modules)
;; Load HFT Research Module
;; ------------------------
(load (expand-file-name "hft-research.el" my/emacs-config-root) nil t)

;; Theming
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'ef-eagle :no-confirm)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '())
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
