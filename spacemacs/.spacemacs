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
   '(clojure
     typescript
     theming
     themes-megapack
     (mu4e :variables
           mu4e-installation-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
           mu4e-enable-notifications t)
     haskell
     ruby-on-rails
     nginx
     racket
     docker
     syntax-checking
     (org :variables
          org-use-speed-commands t)
     (ruby :variables
           ruby-enable-enh-ruby-mode t
           ruby-version-manager 'rvm
           ruby-test-runner 'rspec)
     python
     ipython-notebook
     yaml
     osx
     emacs-lisp
     git
     helm
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org git)
     xkcd
     common-lisp
     javascript
     html
     latex
     markdown
     shell
     shell-scripts
     sql
     typescript
     finance
     ibuffer
     erc
     csv
     restclient
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '((org-roam :location (recipe :fetcher github :repo "jethrokuan/org-roam"))
                                      writeroom-mode
                                      org-super-agenda
                                      flycheck-flow
                                      flow-minor-mode
                                      prettier-js
                                      add-node-modules-path
                                      org-roam
                                      org-analyzer
                                      all-the-icons-dired
                                      all-the-icons
                                      treemacs
                                      treemacs-evil
                                      treemacs-projectile
                                      kaolin-themes
                                      evil-string-inflection
                                      eyebrowse
                                      doom-themes
                                      beacon
                                      string-inflection
                                      helm-org-rifle
                                      mu4e-alert
                                      keyfreq
                                      js2-refactor
                                      yasnippet-snippets
                                      vimish-fold
                                      elpy
                                      company
                                      nvm
                                      multiple-cursors
                                      yasnippet-snippets
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
   dotspacemacs-startup-lists '((agenda . 5)
                                (recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         doom-one
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-mode-line-theme 'doom
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
   dotspacemacs-visual-line-move-text t
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
   dotspacemacs-persistent-server t
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
   dotspacemacs-whitespace-cleanup 'all
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; Load path
  ;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
  (defun update-load-path (&rest _)
    "Update `load-path'."
    (push "~/.emacsconfig/lisp" load-path))

  (setq-default git-magit-status-fullscreen t)

  (update-load-path)

  (require 'init-ui)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (use-package org-roam
    :after org
    :hook (org-mode . org-roam-mode)
    :custom
    (org-roam-directory "~/Dropbox/org/roam/")
    :bind
    ("C-c n l" . org-roam)
    ("C-c n t" . org-roam-today)
    ("C-c n f" . org-roam-find-file)
    ("C-c n i" . org-roam-insert)
    ("C-c n g" . org-roam-show-graph))

(defun writing-mode ()
  (interactive)
  (setq buffer-face-mode-face '(:family "EtBembo" :height 200))
  (buffer-face-mode)
  (linum-mode 0)
  (writeroom-mode 1)
  (blink-cursor-mode)
  (visual-line-mode 1)
  (setq truncate-lines nil)
  (setq line-spacing 5)
  (setq global-hl-line-mode nil))
(add-hook 'markdown-mode-hook 'writing-mode)

  ;; TODO play around w/ prodigy

  (setq evil-org-key-theme '(textobjects navigation additional insert todo))

  (setq olivetti-body-width 0.85)
  ;; (add-hook 'text-mode-hook 'olivetti-mode)

  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  ;; (defun rjsx-mode-init-hook ()
  ;;   (setq-default js2-basic-offset 2)
  ;;   (setq-default js2-strict-missing-semi-warning nil)
  ;;   (setq-default js2-missing-semi-one-line-override t)
  ;;   (setq-default js2-mode-show-strict-warnings nil))
  ;; (add-hook 'rjsx-mode-hook  'rjsx-mode-init-hook)
  ;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (defun web-mode-init-hook ()
    "Hooks for Web mode.  Adjust indent."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (electric-pair-mode)
    )
  (add-hook 'web-mode-hook  'web-mode-init-hook)

  ;; (defun web-mode-init-prettier-hook ()
  ;;   (add-node-modules-path)
  ;;   (prettier-js-mode))

  ;; (add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

  (setq treemacs-use-follow-mode t
        treemacs-use-filewatch-mode t)

  (require 'org-super-agenda)
  (setq org-agenda-sticky nil)

  (org-super-agenda-mode t)
  ;; (doom-modeline-mode 1)
  ;; (setq doom-modeline-icon (display-graphic-p))
  ;; (setq doom-modeline-major-mode-icon t)
  ;; (setq doom-modeline-major-mode-color-icon t)
  ;; (setq doom-modeline-project-detection 'project)
  ;; (setq doom-modeline-buffer-file-name-style 'relative-from-project)

  (define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)
  (define-key evil-normal-state-map (kbd "C-/") 'yas-insert-snippet)
  (define-key evil-insert-state-map (kbd "C-/") 'yas-insert-snippet)

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "C-SPC") #'company-complete-selection))

  (global-set-key (kbd "M-<up>")                (lambda () (interactive) (move-frame-up)))
  (global-set-key (kbd "M-<down>")              (lambda () (interactive) (move-frame-down)))
  (global-set-key (kbd "M-<left>")              (lambda () (interactive) (move-frame-left)))
  (global-set-key (kbd "M-<right>")             (lambda () (interactive) (move-frame-right)))
  (global-set-key (kbd "C-M-S-<down>")          (lambda () (interactive) (enlarge-frame 5)))
  (global-set-key (kbd "C-M-S-<right>")         (lambda () (interactive) (enlarge-frame-horizontally 5)))
  (global-set-key (kbd "C-M-S-<up>")            (lambda () (interactive) (shrink-frame 5)))
  (global-set-key (kbd "C-M-S-<left>")          (lambda () (interactive) (shrink-frame-horizontally 5)))


  ;; Turn off the tildes in the fringe
  (global-vi-tilde-fringe-mode -1)

  (with-eval-after-load 'org
    (setq org-startup-indented t
          org-clock-idle-time 5
          org-ellipsis "  "
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-agenda-block-separator "")

    (setq org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Meetings"
                :tag "meeting")
         (:name "Personal"
                ;; Single arguments given alone
                :file-path "personal"
                :scheduled "before +6m")
         (:name "Investing"
                :file-path "invest")
         (:name "Social"
                :tag "social")
         (:name "MetaMetrics"
                :file-path "metametrics")
         (:name "Idiomatic"
                :file-path "idiomatic")
         (:name "Emacs"
                :tag "emacs")
         (:todo "WAITING" :order 8)  ; Set order of this section
         (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                ;; Show this group at the end of the agenda (since it has the
                ;; highest number). If you specified this group last, items
                ;; with these todo keywords that e.g. have priority A would be
                ;; displayed in that group instead, because items are grouped
                ;; out in the order the groups are listed.
                :order 9)

         ))


    (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))

    )

  (ido-mode -1)

  (scroll-bar-mode -1)

  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq vc-follow-symlinks t)

  (add-hook 'js2-mode-hook #'js2-refactor-mode)

  ;; make sure emacs uses environment variables from my shell
  ;; (when (memq window-system '(mac ns x))
  ;;   (exec-path-from-shell-initialize))

  (setq vc-follow-symlinks t)

  (setq-default dired-listing-switches "-alh")
  (put 'dired-find-alternate-file 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)

  (global-auto-revert-mode t)

  (beacon-mode 1)

  (display-time-mode t)

  (eyebrowse-mode t)

  (setq visible-bell t)

  (require 'flycheck)
  (require 'flycheck-flow)
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; Disable jshint in favor of eslint
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with rjsx-mode for jsx files
  ;; (flycheck-add-mode 'javascript-eslint 'rjsx-mode)

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
    (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))


  (setq helm-ag-use-grep-ignore-list t)
  (setq heml-ag-use-agignore t)

  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  (add-hook 'web-mode-hook 'flow-minor-enable-automatically)
  ;; (add-hook 'rjsx-mode-hook 'flow-minor-enable-automatically)

  ;; (global-company-mode)
  ;; (global-flycheck-mode 1)

  (require 'vimish-fold)
  (spacemacs/set-leader-keys "off" 'vimish-fold)
  (spacemacs/set-leader-keys "ofu" 'vimish-fold-unfold)
  (spacemacs/set-leader-keys "ofd" 'vimish-fold-delete)
  (spacemacs/set-leader-keys "ofr" 'vimish-fold-refold)

  (require 'helm-org-rifle)
  (setq helm-org-rifle-show-path t)

  (delete-selection-mode 1)

  (require 'yasnippet)
  (setq yas-snippet-dirs '("~/.emacsconfig/snippets"))
  (yas-global-mode 1)

  (scroll-bar-mode -1)

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'backward-kill-word))

  (defun interactive-write-function (name args body)
    (interactive "sName: \nsArgs: \nsBody: ")
    (insert (write-function name args body)))

  (defun write-function (name args body)
    (let ((func-template "(defun %s (%s) %s)"))
      (format t func-template name args body)))


  (with-eval-after-load 'org
    (load "~/.emacsconfig/pkgconfig/org.el"))

  (require 'org-tempo)

  (define-abbrev-table 'org-mode-abbrev-table
    '(
      ("sact" "" skel-org-block-plantuml-activity 0)
      ("sblk" "" skel-org-block 0)
      ("sditaa" "" skel-org-block-ditaa 0)
      ("sdot" "" skel-org-block-dot 0)
      ("selisp" "" skel-org-block-elisp 0)
      ("sfor" "" skel-org-block-plantuml-activity-for 0)
      ("sif" "" skel-org-block-plantuml-activity-if 0)
      ("splantuml" "" skel-org-block-plantuml 0)
      ("sseq" "" skel-org-block-plantuml-sequence 0)
      ("tn" "then")
      ("Tn" "Then")
      ("tr" "there")
      ("Tr" "There")
      ("ts" "things")
      ("Ts" "Things")
      ("Tt" "That")
      ("tt" "that")
      ("Tk" "Think")
      ("tk" "think")
      ("Tg" "Thing")
      ("tg" "thing")
      ("Bc" "Because")
      ("bc" "because")
      ("nd" "and")
      ))

  (defun md-strikethrough-region (beginning end)
    (interactive "r")
    (let ((insert-strikethrough-chars-at (lambda (pos)
                                           (goto-char pos)
                                           (insert "~~"))))
      (if (region-active-p)
          (progn
            (funcall insert-strikethrough-chars-at beginning)
            (funcall insert-strikethrough-chars-at (+ end 2))))))

  ;; (load "~/.emacsconfig/pkgconfig/bh.el")
  (load "~/.emacsconfig/pkgconfig/js2.el")
  (load "~/.emacsconfig/pkgconfig/python.el")

  ;; load config variables
  (load "~/.emacsconfig/variables.el")

  ;; load functions
  (load "~/.emacsconfig/functions.el")

  (load "~/.emacsconfig/lib/flow-minor-mode/flow-minor-mode.el")
  (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)

  (load "~/.emacsconfig/lib/frame-cmds/frame-cmds.el")

  (load "~/.emacsconfig/pkgconfig/mu4e.el")
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
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   (quote
    (flow-minor-mode treemacs-projectile treemacs-evil all-the-icons-dired major-mode-hydra pretty-hydra nadvice evil-evilified-state typescript-mode powerline slime scala-mode sbt-mode test-simple loc-changes load-relative faceup pcre2el org-plus-contrib alert log4e gntp magit-popup macrostep skewer-mode simple-httpd js2-mode parent-mode multi projectile dash-docs haml-mode gitignore-mode pos-tip flycheck flx highlight magit git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree pyvenv highlight-indentation polymode deferred request anaphora websocket transient tablist json-mode docker-tramp json-snatcher json-reformat autothemer web-completion-data dash-functional tern restclient know-your-http-well company hydra inflections multiple-cursors paredit eval-sexp-fu cider sesman spinner queue pkg-info parseedn clojure-mode parseclj a epl markdown-mode rust-mode inf-ruby bind-map bind-key yasnippet packed auctex anaconda-mode pythonic f dash s helm avy helm-core async auto-complete popup treemacs pfuture lv string-inflection shrink-path all-the-icons memoize auctex-latexmk projectile-rails mu4e-maildirs-extension mu4e-alert ht keyfreq intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell feature-mode company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode define-word zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xkcd ws-butler winum white-sand-theme which-key web-mode web-beautify volatile-highlights vimish-fold vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toml-mode toc-org tide tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spotify spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slime-company slim-mode seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rjsx-mode reverse-theme reveal-in-osx-finder restclient-helm restart-emacs rebecca-theme realgud rbenv rake rainbow-delimiters railscasts-theme racket-mode racer pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme popwin pony-mode planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pbcopy paradox osx-trash osx-dictionary orgit organic-green-theme org-present org-pomodoro org-mime org-download org-bullets open-junk-file omtose-phellack-theme olivetti oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http nvm noflet noctilux-theme nginx-mode neotree naquadah-theme mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme lush-theme lorem-ipsum livid-mode live-py-mode linum-relative link-hint light-soap-theme ledger-mode launchctl kaolin-themes js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme indent-guide ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers heroku-theme hemisu-theme helm-themes helm-swoop helm-spotify-plus helm-pydoc helm-projectile helm-org-rifle helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme fuzzy flycheck-rust flycheck-pos-tip flycheck-ledger flx-ido flatui-theme flatland-theme fish-mode fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-string-inflection evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks enh-ruby-mode emmet-mode elpy elisp-slime-nav ein dumb-jump dracula-theme doom-themes doom-modeline dockerfile-mode docker django-theme diminish dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode company-web company-tern company-statistics company-shell company-restclient company-auctex company-anaconda common-lisp-snippets column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby cherry-blossom-theme cargo busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))))
 '(variable-pitch ((t (:family "EtBembo" :background nil :height 1.4 :foreground "#1c1e1f")))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(flycheck-javascript-flow-args nil)
 '(package-selected-packages
   (quote
    (writeroom-mode visual-fill-column org-super-agenda ts xterm-color shell-pop prettier-js org-roam emacsql-sqlite3 emacsql org-analyzer multi-term flycheck-flow flow-minor-mode eshell-z eshell-prompt-extras esh-help add-node-modules-path treemacs-projectile treemacs-evil all-the-icons-dired major-mode-hydra pretty-hydra nadvice evil-evilified-state typescript-mode powerline slime scala-mode sbt-mode test-simple loc-changes load-relative faceup pcre2el org-plus-contrib alert log4e gntp magit-popup macrostep skewer-mode simple-httpd js2-mode parent-mode multi projectile dash-docs haml-mode gitignore-mode pos-tip flycheck flx highlight magit git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree pyvenv highlight-indentation polymode deferred request anaphora websocket transient tablist json-mode docker-tramp json-snatcher json-reformat autothemer web-completion-data dash-functional tern restclient know-your-http-well company hydra inflections multiple-cursors paredit eval-sexp-fu cider sesman spinner queue pkg-info parseedn clojure-mode parseclj a epl markdown-mode rust-mode inf-ruby bind-map bind-key yasnippet packed auctex anaconda-mode pythonic f dash s helm avy helm-core async auto-complete popup treemacs pfuture lv string-inflection shrink-path all-the-icons memoize auctex-latexmk projectile-rails mu4e-maildirs-extension mu4e-alert ht keyfreq intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell feature-mode company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode define-word zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xkcd ws-butler winum white-sand-theme which-key web-mode web-beautify volatile-highlights vimish-fold vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toml-mode toc-org tide tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spotify spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slime-company slim-mode seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rjsx-mode reverse-theme reveal-in-osx-finder restclient-helm restart-emacs rebecca-theme realgud rbenv rake rainbow-delimiters railscasts-theme racket-mode racer pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme popwin pony-mode planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pbcopy paradox osx-trash osx-dictionary orgit organic-green-theme org-present org-pomodoro org-mime org-download org-bullets open-junk-file omtose-phellack-theme olivetti oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http nvm noflet noctilux-theme nginx-mode neotree naquadah-theme mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme lush-theme lorem-ipsum livid-mode live-py-mode linum-relative link-hint light-soap-theme ledger-mode launchctl kaolin-themes js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme indent-guide ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers heroku-theme hemisu-theme helm-themes helm-swoop helm-spotify-plus helm-pydoc helm-projectile helm-org-rifle helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme fuzzy flycheck-rust flycheck-pos-tip flycheck-ledger flx-ido flatui-theme flatland-theme fish-mode fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-string-inflection evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks enh-ruby-mode emmet-mode elpy elisp-slime-nav ein dumb-jump dracula-theme doom-themes doom-modeline dockerfile-mode docker django-theme diminish dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode company-web company-tern company-statistics company-shell company-restclient company-auctex company-anaconda common-lisp-snippets column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby cherry-blossom-theme cargo busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))))
 '(variable-pitch ((t (:family "EtBembo" :background nil :height 1.4 :foreground "#1c1e1f")))))
