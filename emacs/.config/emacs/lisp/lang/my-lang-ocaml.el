;;; my-lang-ocaml.el --- OCaml tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Tuareg editing, Merlin integration, dune/utop helpers, and ocamlformat.

;;; Code:

(use-package tuareg-mode
  :straight (:host github :repo "ocaml/tuareg")
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)))

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

(use-package merlin
  :commands merlin-mode
  :hook ((tuareg-mode . merlin-mode)
         (caml-mode . merlin-mode))
  :custom
  (merlin-command 'opam))

(use-package dune
  :straight t
  :mode ("\\`dune\\'" . dune-mode))

(use-package utop
  :straight t
  :hook (tuareg-mode . utop-minor-mode)
  :custom
  (utop-command "opam exec -- utop -emacs"))

(defun my-ocamlformat-buffer ()
  "Format the current OCaml buffer using ocamlformat."
  (interactive)
  (when (and buffer-file-name
             (memq major-mode '(tuareg-mode caml-mode)))
    (let ((exit-code
           (call-process "ocamlformat" nil "*ocamlformat*" nil
                         "--inplace"
                         "--enable-outside-detected-project"
                         "--name" (buffer-name)
                         buffer-file-name)))
      (if (zerop exit-code)
          (progn
            (revert-buffer :ignore-auto :noconfirm)
            (message "Formatted successfully."))
        (message "Ocamlformat failed. Check *ocamlformat* buffer for details.")))))

(add-hook 'tuareg-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'my-ocamlformat-buffer nil t)))

(use-package flycheck-ocaml
  :straight t
  :after (merlin flycheck)
  :config
  (flycheck-ocaml-setup))

(with-eval-after-load 'merlin
  (setq merlin-completion-with-doc t)
  (add-hook 'merlin-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'merlin-company-backend))))

(provide 'my-lang-ocaml)

;;; my-lang-ocaml.el ends here
