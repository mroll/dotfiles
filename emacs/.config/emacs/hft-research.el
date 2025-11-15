;;; hft-research.el --- HFT Research Workflow Configuration -*- lexical-binding: t -*-

;; Author: Matt Roll
;; Description: Configuration for HFT research workflow using org-roam, Citar, and org-mode

;;; Commentary:
;; This module provides keybindings and functions for managing an HFT research workflow.
;; It includes:
;; - Note creation templates (source, concept, experiment, question)
;; - Evil-mode keybindings with Space leader
;; - Bibliography integration with Citar
;; - Search and navigation functions

;;; Code:

(require 'org-roam)
(require 'citar)
(require 'consult)
(require 'general)

;;; Custom Functions
;;; ----------------

(defun hft/search-notes ()
  "Search all HFT notes using consult-ripgrep."
  (interactive)
  (consult-ripgrep "~/notes/hft/"))

(defun hft/search-concepts ()
  "Search only concept notes using consult-ripgrep."
  (interactive)
  (consult-ripgrep "~/notes/hft/concepts/"))

(defun hft/search-experiments ()
  "Search only experiment/lab notes using consult-ripgrep."
  (interactive)
  (consult-ripgrep "~/notes/hft/lab/"))

(defun hft/find-note ()
  "Find a note in the HFT directory."
  (interactive)
  (let ((default-directory "~/notes/hft/"))
    (call-interactively #'find-file)))

(defun hft/new-question ()
  "Quickly capture a question using org-roam."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("q" "question" plain
                                   "* Question\n\n%?\n\n* Context\n\n* Potential Approaches\n\n* References\n\n"
                                   :target (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org"
                                                      "#+title: ${title}\n#+filetags: :question:\n\n")
                                   :unnarrowed t))))

(defun hft/new-concept ()
  "Create a new concept note with template using org-roam."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("c" "concept" plain
                                   "* Definition\n\n%?\n\n* Intuition\n\n* Mathematical Formulation\n\n#+begin_src latex\n% LaTeX equations here\n#+end_src\n\n* Implementation Notes\n\n* Related Concepts\n\n* Sources\n\n"
                                   :target (file+head "concepts/%<%Y%m%d%H%M%S>-${slug}.org"
                                                      "#+title: ${title}\n#+filetags: :concept:\n\n")
                                   :unnarrowed t))))

(defun hft/new-experiment ()
  "Create a new experiment/lab note with template using org-roam."
  (interactive)
  (let ((git-rev (if (file-exists-p "~/exchd/.git")
                     (string-trim
                      (shell-command-to-string "cd ~/exchd && git rev-parse --short HEAD"))
                   "N/A")))
    (org-roam-capture- :node (org-roam-node-create)
                       :templates `(("e" "experiment" plain
                                     ,(format "* Experiment: ${title}\n\n- Date: %%<%Y-%m-%d>\n- Git Rev: %s\n\n* Setup\n\n- Model: \n- Parameters: %%?\n- Seed: \n- Code: \n\n* Hypothesis\n\nWhat do I expect to see, and why?\n\n* Results\n\n* Interpretation\n\n* Next Steps\n\n" git-rev)
                                     :target (file+head "lab/%<%Y%m%d%H%M%S>-${slug}.org"
                                                        "#+title: ${title}\n#+filetags: :experiment:lab:\n\n")
                                     :unnarrowed t)))))

(defun hft/new-source ()
  "Create a new source note using org-roam."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("s" "source" plain
                                   "* Summary\n\n%?\n\n* Key Results\n\n* Important Concepts\n\n* Open Questions\n\n* References\n\n"
                                   :target (file+head "sources/%<%Y%m%d%H%M%S>-${slug}.org"
                                                      "#+title: ${title}\n#+filetags: :source:\n#+author: \n#+year: \n#+bibkey: \n\n")
                                   :unnarrowed t))))

(defun hft/new-source-from-citation ()
  "Create a source note from a citar selection using org-roam."
  (interactive)
  (let* ((keys (citar-select-refs))
         (key (car keys))
         (entry (citar-get-entry key))
         (title (citar-get-value "title" entry))
         (author (citar-get-value "author" entry))
         (year (citar-get-value "year" entry)))
    (org-roam-capture-
     :node (org-roam-node-create :title (format "%s (%s)" title year))
     :props (list :immediate-finish nil)
     :templates `(("s" "source" plain
                   ,(format "* Summary\n\n%%?\n\n* Key Results\n\n* Important Concepts\n\n* Open Questions\n\n* References\n\n")
                   :target (file+head ,(format "sources/%%<%Y%m%d%H%M%S>-%s.org" (org-roam-node-slug (org-roam-node-create :title title)))
                                      ,(format "#+title: %s\n#+filetags: :source:\n#+author: %s\n#+year: %s\n#+bibkey: %s\n\n" title author year key))
                   :unnarrowed t)))))

;;; Agenda Configuration
;;; --------------------

(defun hft/agenda ()
  "Show HFT-specific agenda view."
  (interactive)
  (let ((org-agenda-files (directory-files-recursively "~/notes/hft/projects/" "\\.org$")))
    (org-agenda nil "a")))

;;; Keybindings
;;; -----------

;; HFT Research bindings under SPC n (notes)
(my-leader-def
  :keymaps 'override
  :states 'normal

  "n"   '(:ignore t :which-key "notes")
  "nq"  '(hft/new-question :which-key "quick question")
  "nc"  '(hft/new-concept :which-key "new concept")
  "ne"  '(hft/new-experiment :which-key "new experiment")
  "ns"  '(hft/new-source :which-key "new source")
  "nf"  '(org-roam-node-find :which-key "find node")
  "ni"  '(org-roam-node-insert :which-key "insert link")
  "nb"  '(org-roam-buffer-toggle :which-key "toggle backlinks")
  "na"  '(hft/agenda :which-key "HFT agenda")

  ;; Search commands under SPC s
  "sn"  '(hft/search-notes :which-key "search all notes")
  "sN"  '(hft/search-concepts :which-key "search concepts")
  "sx"  '(hft/search-experiments :which-key "search experiments")

  ;; File commands under SPC f
  "fn"  '(hft/find-note :which-key "find note")

  ;; Bibliography commands under SPC r (references)
  "r"   '(:ignore t :which-key "references")
  "rr"  '(citar-open :which-key "open citar")
  "ri"  '(citar-insert-citation :which-key "insert citation")
  "ro"  '(citar-open-files :which-key "open PDF")
  "rn"  '(hft/new-source-from-citation :which-key "new source note")

  ;; Toggle commands under SPC t (add to existing t prefix)
  "t"   '(:ignore t :which-key "toggle")
  "to"  '(olivetti-mode :which-key "olivetti (focused reading)"))

;; Org-mode local bindings with comma leader
(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map

  "ll" '(org-roam-node-insert :which-key "insert roam link")
  "lb" '(org-roam-buffer-toggle :which-key "toggle backlinks")
  "la" '(org-roam-alias-add :which-key "add alias")
  "lt" '(org-roam-tag-add :which-key "add tag")

  "ci" '(citar-insert-citation :which-key "insert citation")
  "co" '(citar-open-files :which-key "open citation PDF")

  "x"  '(org-babel-execute-src-block :which-key "execute code block"))

(provide 'hft-research)
;;; hft-research.el ends here
