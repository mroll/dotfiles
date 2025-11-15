;;; hft-research.el --- HFT Research Workflow Configuration -*- lexical-binding: t -*-

;; Author: Matt Roll
;; Description: Configuration for HFT research workflow using Denote, Citar, and org-mode

;;; Commentary:
;; This module provides keybindings and functions for managing an HFT research workflow.
;; It includes:
;; - Note creation templates (source, concept, experiment, question)
;; - Evil-mode keybindings with Space leader
;; - Bibliography integration with Citar
;; - Search and navigation functions

;;; Code:

(require 'denote)
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
  "Quickly capture a question to inbox/."
  (interactive)
  (let* ((title (read-string "Question: "))
         (denote-directory "~/notes/hft/inbox/"))
    (denote title '("question"))
    (insert "* Question\n\n")
    (insert (format "%s\n\n" title))
    (insert "* Context\n\n")
    (insert "* Potential Approaches\n\n")
    (insert "* References\n\n")
    (goto-char (point-min))
    (forward-line 3)))

(defun hft/new-concept ()
  "Create a new concept note with template."
  (interactive)
  (let* ((title (read-string "Concept name: "))
         (keywords (completing-read-multiple "Keywords: " denote-known-keywords))
         (denote-directory "~/notes/hft/concepts/"))
    (denote title (cons "concept" keywords))
    (insert "* Definition\n\n")
    (insert "* Intuition\n\n")
    (insert "* Mathematical Formulation\n\n")
    (insert "#+begin_src latex\n")
    (insert "% LaTeX equations here\n")
    (insert "#+end_src\n\n")
    (insert "* Implementation Notes\n\n")
    (insert "* Related Concepts\n\n")
    (insert "* Sources\n\n")
    (goto-char (point-min))
    (forward-line 2)))

(defun hft/new-experiment ()
  "Create a new experiment/lab note with template."
  (interactive)
  (let* ((title (read-string "Experiment description: "))
         (keywords (completing-read-multiple "Keywords: " denote-known-keywords))
         (denote-directory "~/notes/hft/lab/")
         (git-rev (if (file-exists-p "~/exchd/.git")
                      (string-trim
                       (shell-command-to-string "cd ~/exchd && git rev-parse --short HEAD"))
                    "N/A")))
    (denote title (cons "experiment" keywords))
    (insert (format "* Experiment: %s\n\n" title))
    (insert (format "- Date: %s\n" (format-time-string "%Y-%m-%d")))
    (insert (format "- Git Rev: %s\n" git-rev))
    (insert "\n* Setup\n\n")
    (insert "- Model: \n")
    (insert "- Parameters: \n")
    (insert "- Seed: \n")
    (insert "- Code: \n")
    (insert "\n* Hypothesis\n\n")
    (insert "What do I expect to see, and why?\n\n")
    (insert "* Results\n\n")
    (insert "* Interpretation\n\n")
    (insert "* Next Steps\n\n")
    (goto-char (point-min))
    (search-forward "Model: ")
    (end-of-line)))

(defun hft/new-source-from-citation ()
  "Create a source note from a citar selection."
  (interactive)
  (let* ((keys (citar-select-refs))
         (key (car keys))
         (entry (citar-get-entry key))
         (title (citar-get-value "title" entry))
         (author (citar-get-value "author" entry))
         (year (citar-get-value "year" entry))
         (denote-directory "~/notes/hft/sources/"))
    (denote (format "%s (%s)" title year) '("source"))
    (insert (format "* %s\n\n" title))
    (insert (format "- Authors: %s\n" author))
    (insert (format "- Year: %s\n" year))
    (insert (format "- Bibkey: cite:%s\n" key))
    (insert "- Status: unread\n")
    (insert "- Tags: \n")
    (insert "\n* Summary\n\n")
    (insert "* Key Results\n\n")
    (insert "* Important Concepts\n\n")
    (insert "* Open Questions\n\n")
    (insert "* References\n\n")
    (goto-char (point-min))
    (search-forward "Summary")
    (forward-line 2)))

(defun hft/insert-denote-link ()
  "Insert a denote link with completion."
  (interactive)
  (call-interactively #'denote-link))

(defun hft/link-to-concept ()
  "Insert a denote link, scoped to concepts/ directory."
  (interactive)
  (let ((denote-directory "~/notes/hft/concepts/"))
    (call-interactively #'denote-link)))

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
  "nl"  '(hft/insert-denote-link :which-key "insert link")
  "nb"  '(denote-backlinks :which-key "show backlinks")
  "nr"  '(denote-rename-file :which-key "rename note")
  "na"  '(hft/agenda :which-key "HFT agenda")

  ;; Search commands under SPC s
  "s"   '(:ignore t :which-key "search")
  "sn"  '(hft/search-notes :which-key "search all notes")
  "sN"  '(hft/search-concepts :which-key "search concepts")
  "sx"  '(hft/search-experiments :which-key "search experiments")

  ;; File commands under SPC f
  "fn"  '(hft/find-note :which-key "find note")
  "fN"  '(denote-open-or-create :which-key "denote find/create")

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

  "ll" '(hft/insert-denote-link :which-key "insert link")
  "lc" '(hft/link-to-concept :which-key "link to concept")
  "lb" '(denote-backlinks :which-key "show backlinks")

  "ci" '(citar-insert-citation :which-key "insert citation")
  "co" '(citar-open-files :which-key "open citation PDF")

  "x"  '(org-babel-execute-src-block :which-key "execute code block"))

;;; Denote Templates
;;; ----------------

;; You can extend these with denote's template system if needed
;; For now, the functions above provide the templates inline

(provide 'hft-research)
;;; hft-research.el ends here
