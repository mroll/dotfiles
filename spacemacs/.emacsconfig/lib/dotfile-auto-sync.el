;;; dotfile-auto-sync.el --- -*- lexical-binding: t -*-

;; Copyright © 2020 Matt Roll

;; Author: Matt Roll <mproll@pm.me>
;; Version: 0.0.1
;; Keywords: dotfiles, git

;; This file is not part of GNU Emacs.

;; This program is Free Software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; tbd

;;; History

;; 0.0.1, 17 March 2020
;;   Initial relase

;;; Code:



;; ======================================================================




(defun dotfile-auto-sync-start ()
  )


(defcustom dotfile-auto-sync-repo-path "~/dotfiles"
  "Path to the dotfiles repo.")


(defun das--pull-and-rebase ()
  "Pull new changes from remote origin."
  (with-helm-default-directory dotfile-auto-sync-repo-path
    (vc-git-pull nil)))



(das--pull-and-rebase)

(provide 'dotfile-auto-async)

;;; dotfile-auto-sync ends here
