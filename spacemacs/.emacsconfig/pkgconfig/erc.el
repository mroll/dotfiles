(set-face-attribute 'erc-input-face nil :inherit 'variable-pitch :foreground "DeepSkyBlue1" :height 1.2 :family "Inconsolata")

(defun mpr/erc-connect ()
  (erc :server "matthewroll.com" :port 5000 :nick "le4fy" :password mpr/znc-pass))
