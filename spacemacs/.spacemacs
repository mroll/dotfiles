;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     theming
     themes-megapack
     nginx
     (scala
      :variables scala-indent:indent-value-expression t
      scala-indent:align-forms nil
      scala-indent:align-parameters nil)
     racket
     docker
     clojure
     dash
     syntax-checking
     spotify
     (ruby :variables ruby-enable-enh-ruby-mode t)
     (org :variables
          org-use-speed-commands t)
     python
     ipython-notebook
     rust
     yaml
     osx
     emacs-lisp
     git
     helm
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org git)
     django
     xkcd
     common-lisp
     javascript
     html
     latex
     markdown
     shell-scripts
     sql
     typescript
     finance
     gnus
     ibuffer
     erc
     csv
     restclient
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      olivetti
                                      kaolin-themes
                                      doom-modeline
                                      evil-string-inflection
                                      eyebrowse
                                      doom-themes
                                      beacon
                                      string-inflection
                                      helm-org-rifle
                                      yasnippet-snippets
                                      vimish-fold
                                      elpy
                                      eww
                                      company
                                      realgud
                                      nvm
                                      rjsx-mode
                                      multiple-cursors
                                      hy-mode
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(org-projectile)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         spacemacs-light
                         white-sand
                         dorsey
                         hemisu-light
                         phoenix-dark-pink
                         subatomic
                         underwater
                         rebecca
                         noctilux
                         ample-flat
                         bubbleberry
                         phoenix-dark-pink
                         omtose-darker
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Inconsolata"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-mode-line-theme 'spacemacs
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq theming-modifications
        '((spacemacs-light
           (variable-pitch :family "EtBembo"
                           :background nil
                           :height 1.4
                           :foreground "#1c1e1f")
           )))

  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)
  (push '("ensime" . "melpa-stable") package-pinned-packages)

  (setq-default git-magit-status-fullscreen t)

  (display-time-mode 1)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (setq olivetti-body-width 0.85)
  ;; (add-hook 'text-mode-hook 'olivetti-mode)

  (doom-modeline-mode 1)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)

  (require 'org-tempo)

  ;; (load "~/.emacsconfig/lib/org-variable-pitch.el")
  ;; (require 'org-variable-pitch)
  ;; (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)
  ;; (setq org-variable-pitch-fixed-font '(:family "Inconsolata"
  ;;                                               :height 1
  ;;                                               :weight normal
  ;;                                               :width normal
  ;;                                               :powerline-scale 1.1))
  ;; (setq org-variable-pitch-fixed-faces
  ;;       '(org-block
  ;;         org-block-begin-line
  ;;         org-block-end-line
  ;;         org-code
  ;;         org-document-info-keyword
  ;;         org-done
  ;;         org-formula
  ;;         org-indent
  ;;         org-meta-line
  ;;         org-special-keyword
  ;;         org-table
  ;;         org-todo
  ;;         org-verbatim
  ;;         org-date

  ;;         ;; additional
  ;;         org-level-1
  ;;         org-level-2
  ;;         org-level-3
  ;;         org-level-4
  ;;         org-level-5
  ;;         org-level-6
  ;;         org-level-7
  ;;         org-level-8))

  ;; (load "~/.emacsconfig/lib/docker-explorer.el")

  ;; Turn off the tildes in the fringe
  (global-vi-tilde-fringe-mode -1)

  (with-eval-after-load 'org
    (setq org-startup-indented t
          org-clock-idle-time 5
          org-ellipsis "  "
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-agenda-block-separator ""))
          ;; org-fontify-whole-heading-line t
          ;; org-fontify-done-headline t
          ;; org-fontify-quote-and-verse-blocks t))

  (ido-mode -1)

  ;; (tramp-set-completion-function "ssh"
  ;;                                '((tramp-parse-sconfig "/etc/ssh_config")
  ;;                                  (tramp-parse-sconfig "~/.ssh/config")))

  ;; make sure emacs uses environment variables from my shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  ;; (setq ensime-startup-notification nil)

  (setq vc-follow-symlinks t)

  (setq-default dired-listing-switches "-alh")
  (put 'dired-find-alternate-file 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)

  (global-auto-revert-mode t)

  (beacon-mode 1)

  (display-time-mode t)

  (eyebrowse-mode t)

  (setq visible-bell t)

  ;; (defun things ()
  ;;   "Open main 'org-mode' file and start 'org-agenda' for today."
  ;;   (interactive)
  ;;   (find-file (concat org-directory "things.org"))
  ;;   (set-org-agenda-files)
  ;;   (org-agenda-list)
  ;;   (org-agenda-day-view)
  ;;   (shrink-window-if-larger-than-buffer)
  ;;   (other-window 1))

  ;; (setq-default flycheck-scalastylerc "/usr/local/etc/scalastyle_config.xml")
  (setq flycheck-scalastyle-jar
        "/usr/local/Cellar/scalastyle/1.0.0/libexec/scalastyle_2.12-1.0.0-batch.jar")
  (setq flycheck-scalastylerc
        "/usr/local/etc/scalastyle_config.xml")

  (setq scala-indent:align-parameters nil)

  (setq helm-ag-use-grep-ignore-list t)
  (setq heml-ag-use-agignore t)

  (global-company-mode)
  (global-flycheck-mode 1)

  (require 'vimish-fold)
  (spacemacs/set-leader-keys "off" 'vimish-fold)
  (spacemacs/set-leader-keys "ofu" 'vimish-fold-unfold)
  (spacemacs/set-leader-keys "ofd" 'vimish-fold-delete)
  (spacemacs/set-leader-keys "ofr" 'vimish-fold-refold)

  (require 'helm-org-rifle)
  (setq helm-org-rifle-show-path t)

  (require 'yasnippet)
  (setq yas-snippet-dirs '("~/.emacsconfig/snippets"))
  (yas-global-mode 1)

  (scroll-bar-mode -1)

  (defun insert-db-block (dbname)
    (interactive "sDB Name: ")
    (insert (concat dbname ".withConnection { implicit conn => {
\t
}}")))

  ;; load secret variables
  ;; (load "~/.emacsconfig/mpr-secrets.el.gpg")

  ;; load obscure csv library
  ;; (load "~/.emacsconfig/lib/el-csv/parse-csv.el")

  ;; (load "~/.emacsconfig/lib/ob-restclient.el/ob-restclient.el")

  ;; load frame-cmds library
  ;; (load "~/.emacsconfig/lib/frame-cmds/frame-cmds.el")

  (defun wrap-in-db-conn (start end dbname)
    (interactive "r\nsdbname: ")
    (let ((selection (buffer-substring start end)))
      (delete-region start end)
      (insert (format "%s.withConnection { implicit conn => {\n" dbname))
      (indent-for-tab-command)
      (insert selection)
      (insert "\n")
      (indent-for-tab-command)
      (insert "}}")
      (indent-for-tab-command)))

  (defun implicit-conn-arg ()
    (interactive)
    (forward-char)
    (insert "(implicit conn: Connection)"))

  ;; load config variables
  (load "~/.emacsconfig/variables.el")

  ;; load functions
  (load "~/.emacsconfig/functions.el")

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'backward-kill-word))

  ;; load keybindings
  (load "~/.emacsconfig/keys.el")

  (with-eval-after-load 'erc
    (load "~/.emacsconfig/pkgconfig/erc.el"))

  (setq ensime-startup-notification nil)

  ;; (with-eval-after-load 'gnus
  ;;   (load "~/.emacsconfig/pkgconfig/gnus.el"))

  ;; Get email, and store in nnml
  ;; (setq gnus-secondary-select-methods
  ;;       '(
  ;;         (nntp "gmane"
  ;;               (nntp-address "news.gmane.org"))
  ;;         (nntp "news.eternal-september.org")
  ;;         (nntp "nntp.aioe.org")
  ;;         (nntp "news.gwene.org")
  ;;         ))

  (with-eval-after-load 'js2
    (load "~/.emacsconfig/pkgconfig/js2.el"))

  (with-eval-after-load 'tide
    (load "~/.emacsconfig/pkgconfig/tide.el"))

  (with-eval-after-load 'org
    (load "~/.emacsconfig/pkgconfig/bh.el")
    (load "~/.emacsconfig/pkgconfig/org.el"))

  (setq org-use-speed-commands t)

  (load "~/.emacsconfig/pkgconfig/bh.el")
  (load "~/.emacsconfig/pkgconfig/org.el")
  (load "~/.emacsconfig/pkgconfig/js2.el")

  ;; TODO: get this working
  ;; (defadvice org-goto (around enter-insert-mode activate compile)
  ;;   ad-do-it
  ;;   (evil-insert-state))

  (with-eval-after-load 'python
    (load "~/.emacsconfig/pkgconfig/python.el"))

  (add-hook 'realgud-short-key-mode-hook
            (lambda ()
              (local-set-key "\C-c" realgud:shortkey-mode-map)))

  (defadvice org-goto (around make-it-evil activate)
    (let ((orig-state evil-state)
          (evil-emacs-state-modes (cons 'org-mode evil-emacs-state-modes)))
      ad-do-it
      (evil-change-state orig-state)))

  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elpy find-file-in-project helm-dash flycheck-rust flycheck-pos-tip flycheck-ledger dash-at-point spotify helm-spotify-plus multi rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby auctex-latexmk yapfify yaml-mode xkcd ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tide typescript-mode flycheck tagedit sql-indent spaceline powerline smeargle slime-company slime slim-mode scss-mode sass-mode rjsx-mode reveal-in-osx-finder restclient-helm restart-emacs realgud test-simple loc-changes load-relative ranger rainbow-delimiters racer pos-tip pyvenv pytest pyenv-mode py-isort pug-mode popwin pony-mode pip-requirements persp-mode pcre2el pbcopy paradox spinner osx-trash osx-dictionary orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file ob-restclient ob-http nvm neotree move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode ledger-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js-doc insert-shebang indent-guide ibuffer-projectile hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit ghub let-alist with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emmet-mode elisp-slime-nav ein skewer-mode request-deferred websocket request deferred js2-mode simple-httpd dumb-jump diminish cython-mode csv-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-restclient restclient know-your-http-well company-auctex company-anaconda company common-lisp-snippets column-enforce-mode coffee-mode clean-aindent-mode cargo rust-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup omtose-phellack-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(ansi-term-color-vector
   [unspecified "#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"] t)
 '(background-color "#202020")
 '(background-mode dark)
 '(beacon-color "#F8BBD0")
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#cccccc")
 '(cursor-type (quote bar))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)) t)
 '(evil-insert-state-cursor (quote ("#D50000" bar)) t)
 '(evil-normal-state-cursor (quote ("#F57F17" box)) t)
 '(evil-visual-state-cursor (quote ("#66BB6A" box)) t)
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#202325" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(foreground-color "#cccccc")
 '(frame-background-mode (quote dark))
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 6 nil (fringe))
 '(gnus-logo-colors (quote ("#0d7b72" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (quote
    ("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors
   (quote
    (("#424748" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#424748" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")) t)
 '(hl-sexp-background-color "#1c1f26")
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(main-line-separator-style (quote chamfer))
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(objed-cursor-color "#ff5555")
 '(org-src-block-faces (quote (("emacs-lisp" (:background "#F0FFF0")))))
 '(package-selected-packages
   (quote
    (kaolin-valley-light-theme olivetti kaolin-themes doom-modeline shrink-path all-the-icons memoize doom-dracula-theme anaphora org-roam emacsql-sqlite emacsql evil-string-inflection doom-nord-light-theme doom-themes dash-docs transient polymode lv parseedn parseclj a beacon string-inflection flx-ido treepy graphql ample-theme helm-gtags ggtags helm-org-rifle zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme alect-themes afternoon-theme stekene-theme sesman nginx-mode enh-ruby-mode racket-mode faceup yasnippet-snippets vimish-fold noflet ensime sbt-mode scala-mode dockerfile-mode docker tablist docker-tramp clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider seq queue clojure-mode elpy find-file-in-project ivy helm-dash flycheck-rust flycheck-pos-tip flycheck-ledger dash-at-point spotify helm-spotify-plus multi rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby auctex-latexmk yapfify yaml-mode xkcd ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tide typescript-mode flycheck tagedit sql-indent spaceline powerline smeargle slime-company slime slim-mode scss-mode sass-mode rjsx-mode reveal-in-osx-finder restclient-helm restart-emacs realgud test-simple loc-changes load-relative ranger rainbow-delimiters racer pos-tip pyvenv pytest pyenv-mode py-isort pug-mode popwin pony-mode pip-requirements persp-mode pcre2el pbcopy paradox spinner osx-trash osx-dictionary orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file ob-restclient ob-http nvm neotree move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode ledger-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js-doc insert-shebang indent-guide ibuffer-projectile hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit ghub let-alist with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emmet-mode elisp-slime-nav ein skewer-mode request-deferred websocket request deferred js2-mode simple-httpd dumb-jump diminish cython-mode csv-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-restclient restclient know-your-http-well company-auctex company-anaconda company common-lisp-snippets column-enforce-mode coffee-mode clean-aindent-mode cargo rust-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup omtose-phellack-theme)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(pos-tip-background-color "#414E63")
 '(pos-tip-foreground-color "#BEC8DB")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(tabbar-background-color "#ffffffffffff")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#1f2124")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff0000")
     (40 . "#ff4a52")
     (60 . "#f6aa11")
     (80 . "#f1e94b")
     (100 . "#f5f080")
     (120 . "#f6f080")
     (140 . "#41a83e")
     (160 . "#40b83e")
     (180 . "#b6d877")
     (200 . "#b7d877")
     (220 . "#b8d977")
     (240 . "#b9d977")
     (260 . "#93e0e3")
     (280 . "#72aaca")
     (300 . "#8996a8")
     (320 . "#afc4db")
     (340 . "#cfe2f2")
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3")
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))))
 '(xterm-color-names
   ["#414E63" "#CC71D1" "#88D6CB" "#C79474" "#76A2D1" "#4A4B6B" "#96A9D6" "#8E95A3"])
 '(xterm-color-names-bright
   ["#555B77" "#E074DB" "#8BE8D8" "#B2DEC1" "#75B5EB" "#9198EB" "#C3C3E8" "#838791"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button))))))
