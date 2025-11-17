;;; my-module-org-roam.el --- Org-roam configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-roam directories, templates, and completion tweaks.

;;; Code:

(use-package sqlite3
  :straight t)

(use-package org-roam
  :straight t
  :after org
  :config
  (setq org-roam-directory (file-truename "~/org-roam/"))
  (org-roam-db-autosync-mode)
  ;; (setq org-roam-capture-templates
  ;;       '(("d" "default" plain "%?"
  ;;          :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
  ;;                             "#+title: ${title}\n#+filetags: \n\n")
  ;;          :unnarrowed t)
  ;;         ("c" "concept" plain
  ;;          "* Definition\n\n%?\n\n* Intuition\n\n* Mathematical Formulation\n\n#+begin_src latex\n% LaTeX equations here\n#+end_src\n\n* Implementation Notes\n\n* Related Concepts\n\n* Sources\n\n"
  ;;          :target (file+head "concepts/%<%Y%m%d%H%M%S>-${slug}.org"
  ;;                             "#+title: ${title}\n#+filetags: :concept:\n\n")
  ;;          :unnarrowed t)
  ;;         ("s" "source" plain
  ;;          "* Summary\n\n%?\n\n* Key Results\n\n* Important Concepts\n\n* Open Questions\n\n* References\n\n"
  ;;          :target (file+head "sources/%<%Y%m%d%H%M%S>-${slug}.org"
  ;;                             "#+title: ${title}\n#+filetags: :source:\n#+author: \n#+year: \n#+bibkey: \n\n")
  ;;          :unnarrowed t)
  ;;         ("e" "experiment" plain
  ;;          "* Experiment: ${title}\n\n- Date: %<%Y-%m-%d>\n- Git Rev: \n\n* Setup\n\n- Model: \n- Parameters: %?\n- Seed: \n- Code: \n\n* Hypothesis\n\nWhat do I expect to see, and why?\n\n* Results\n\n* Interpretation\n\n* Next Steps\n\n"
  ;;          :target (file+head "lab/%<%Y%m%d%H%M%S>-${slug}.org"
  ;;                             "#+title: ${title}\n#+filetags: :experiment:lab:\n\n")
  ;;          :unnarrowed t)
  ;;         ("q" "question" plain
  ;;          "* Question\n\n%?\n\n* Context\n\n* Potential Approaches\n\n* References\n\n"
  ;;          :target (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org"
  ;;                             "#+title: ${title}\n#+filetags: :question:\n\n")
  ;;          :unnarroved t)))
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-completion-everywhere t))

(provide 'my-module-org-roam)

;;; my-module-org-roam.el ends here
