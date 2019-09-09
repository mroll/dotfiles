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
     (mu4e :variables
           mu4e-installation-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
           mu4e-enable-notifications t)
     themes-megapack
     haskell
     ruby-on-rails
     nginx
     scala
     racket
     docker
     clojure
     dash
     syntax-checking
     spotify
     (ruby :variables ruby-enable-enh-ruby-mode t
           ruby-version-manager 'rvm)
     org
     ranger
     python
     ipython-notebook
     rust
     yaml
     osx
     emacs-lisp
     git
     helm
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t)
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
                                      mu4e-alert
                                      keyfreq
                                      js2-refactor
                                      intero
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
                         twilight-bright
                         sanityinc-solarized-light
                         hemisu-light
                         underwater
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

  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)
  (push '("ensime" . "melpa-stable") package-pinned-packages)

  (setq-default git-magit-status-fullscreen t)

  (setq-default epa-pinentry-mode 'loopback)
  (pinentry-start)
  (display-time-mode 1)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (display-time-mode t)
  (scroll-bar-mode -1)

  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq vc-follow-symlinks t)

  (setq mail-user-agent 'mu4e-user-agent)
  ;; default
  (setq mu4e-maildir "~/.mail/pm")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-trash-folder  "/Trash")
  (setq mu4e-refile-folder  "/All")

  (setq mu4e-headers-fields
        '( (:human-date       .   12)
           (:flags            .    6)
           (:mailing-list     .   10)
           (:from             .   22)
           (:thread-subject   .   nil)))

  (setq mu4e-search-result-limit 5000)
  (setq message-kill-buffer-on-exit t)

  (mu4e-alert-set-default-style 'notifier)
  (mu4e-alert-enable-mode-line-display)

  ;; This allows me to use 'helm' to select mailboxes
  (setq mu4e-completing-read-function 'completing-read)
  ;; Why would I want to leave my message open after I've sent it?
  (setq message-kill-buffer-on-exit t)
  ;; Don't ask for a 'context' upon opening mu4e
  (setq mu4e-context-policy 'pick-first)
  ;; Don't ask to quit... why is this the default?
  (setq mu4e-confirm-quit nil)

  (setq user-mail-address "mproll@pm.me")
  (setq smtpmail-default-smtp-server "127.0.0.1"
        smtpmail-smtp-server "127.0.0.1"
        smtpmail-smtp-service 1025)
  (setq message-send-mail-function 'smtpmail-send-it)

  (setq mu4e-update-interval 300)
  (setq mu4e-view-show-addresses 't)

  (setq mu4e-get-mail-command "offlineimap")

  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"                       . ?i)
           ("/Sent"                        . ?s)
           ("/Trash"                       . ?T)
           ("/Folders.travel"              . ?t)
           ("/Folders.finance"             . ?f)
           ("/Folders.receipts"            . ?r)
           ("/Folders.important.contacts"  . ?c)
           ("/Folders.subscriptions"       . ?u)
           ("/Folders.projects"            . ?p)))

  (add-hook 'js2-mode-hook #'js2-refactor-mode)

  ;; make sure emacs uses environment variables from my shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  (setq ensime-startup-notification nil)

  (setq flycheck-scalastyle-jar
        "/usr/local/Cellar/scalastyle/0.8.0/libexec/scalastyle_2.11-0.8.0-batch.jar")
  (setq flycheck-scalastylerc
        "/usr/local/etc/scalastyle_config.xml")

  (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))

  (setq-default flycheck-temp-prefix ".flycheck")

  (flycheck-add-mode 'javascript-eslint 'javascript-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)

  (global-company-mode)
  ; (global-flycheck-mode 1)

  (require 'vimish-fold)
  (spacemacs/set-leader-keys "off" 'vimish-fold)
  (spacemacs/set-leader-keys "ofu" 'vimish-fold-unfold)
  (spacemacs/set-leader-keys "ofd" 'vimish-fold-delete)
  (spacemacs/set-leader-keys "ofr" 'vimish-fold-refold)

  (delete-selection-mode 1)

  (require 'yasnippet)
  (setq yas-snippet-dirs '("~/.emacsconfig/snippets"))
  (yas-global-mode 1)

  ;; load secret variables
  ;; (load "~/.emacsconfig/mpr-secrets.el.gpg")

  ;; load obscure csv library
  (load "~/.emacsconfig/lib/el-csv/parse-csv.el")

  (load "~/.emacsconfig/lib/ob-restclient.el/ob-restclient.el")

  ;; load frame-cmds library
  (load "~/.emacsconfig/lib/frame-cmds/frame-cmds.el")

  ;; load config variables
  (load "~/.emacsconfig/variables.el")

  ;; load functions
  (load "~/.emacsconfig/functions.el")

  ;; load keybindings
  (load "~/.emacsconfig/keys.el")

  (with-eval-after-load 'erc
    (load "~/.emacsconfig/pkgconfig/erc.el"))

  (setq abbrev-file-name "~/.emacsconfig/abbrev")

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


  (defun div-with-class ()
    (interactive)
    (insert "<div class=\"\"></div>")
    (backward-char 8))

  (add-hook 'web-mode-hook
            (lambda ()
              (local-set-key (kbd "\C-c \C-d") (quote div-with-class))))

  (defun md-strikethrough-region (beginning end)
    (interactive "r")
    (let ((insert-strikethrough-chars-at (lambda (pos)
                                           (goto-char pos)
                                           (insert "~~"))))
      (if (region-active-p)
          (progn
            (funcall insert-strikethrough-chars-at beginning)
            (funcall insert-strikethrough-chars-at (+ end 2))))))

  (with-eval-after-load 'gnus
    (load "~/.emacsconfig/pkgconfig/gnus.el"))

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

  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("src\\/.*\\.js\\'" . rjsx-mode))

  (with-eval-after-load 'tide
    (load "~/.emacsconfig/pkgconfig/tide.el"))

  (with-eval-after-load 'org
    (load "~/.emacsconfig/pkgconfig/bh.el")
    (load "~/.emacsconfig/pkgconfig/org.el"))

  (with-eval-after-load 'python
    (load "~/.emacsconfig/pkgconfig/python.el"))

  (add-hook 'realgud-short-key-mode-hook
            (lambda ()
              (local-set-key "\C-c" realgud:shortkey-mode-map)))

  (add-hook 'haskell-mode-hook 'intero-mode)
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
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (elpy find-file-in-project helm-dash flycheck-rust flycheck-pos-tip flycheck-ledger dash-at-point spotify helm-spotify-plus multi rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby auctex-latexmk yapfify yaml-mode xkcd ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tide typescript-mode flycheck tagedit sql-indent spaceline powerline smeargle slime-company slime slim-mode scss-mode sass-mode rjsx-mode reveal-in-osx-finder restclient-helm restart-emacs realgud test-simple loc-changes load-relative ranger rainbow-delimiters racer pos-tip pyvenv pytest pyenv-mode py-isort pug-mode popwin pony-mode pip-requirements persp-mode pcre2el pbcopy paradox spinner osx-trash osx-dictionary orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file ob-restclient ob-http nvm neotree move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode ledger-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js-doc insert-shebang indent-guide ibuffer-projectile hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit ghub let-alist with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emmet-mode elisp-slime-nav ein skewer-mode request-deferred websocket request deferred js2-mode simple-httpd dumb-jump diminish cython-mode csv-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-restclient restclient know-your-http-well company-auctex company-anaconda company common-lisp-snippets column-enforce-mode coffee-mode clean-aindent-mode cargo rust-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup omtose-phellack-theme))))
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#BBBBBB" t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("/Users/matt/Dropbox/org/project/artshow2018.org" "/Users/matt/Dropbox/org/project/atla.org" "/Users/matt/Dropbox/org/project/hpmysql.org" "/Users/matt/Dropbox/org/project/webmaker.org" "~/Dropbox/org/agenda-refile-beorg.org")))
 '(package-selected-packages
   (quote
    (mu4e-alert mu4e-maildirs-extension ht keyfreq darktooth-theme zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme company-ghc intero hlint-refactor hindent helm-hoogle haskell-snippets ghc flycheck-haskell company-ghci haskell-mode company-cabal cmm-mode projectile-rails feature-mode nginx-mode enh-ruby-mode racket-mode faceup yasnippet-snippets vimish-fold noflet ensime sbt-mode scala-mode dockerfile-mode docker tablist docker-tramp clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider seq queue clojure-mode elpy find-file-in-project ivy helm-dash flycheck-rust flycheck-pos-tip flycheck-ledger dash-at-point spotify helm-spotify-plus multi rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby auctex-latexmk yapfify yaml-mode xkcd ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tide typescript-mode flycheck tagedit sql-indent spaceline powerline smeargle slime-company slime slim-mode scss-mode sass-mode rjsx-mode reveal-in-osx-finder restclient-helm restart-emacs realgud test-simple loc-changes load-relative ranger rainbow-delimiters racer pos-tip pyvenv pytest pyenv-mode py-isort pug-mode popwin pony-mode pip-requirements persp-mode pcre2el pbcopy paradox spinner osx-trash osx-dictionary orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file ob-restclient ob-http nvm neotree move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode ledger-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js-doc insert-shebang indent-guide ibuffer-projectile hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit ghub let-alist with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emmet-mode elisp-slime-nav ein skewer-mode request-deferred websocket request deferred js2-mode simple-httpd dumb-jump diminish cython-mode csv-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-restclient restclient know-your-http-well company-auctex company-anaconda company common-lisp-snippets column-enforce-mode coffee-mode clean-aindent-mode cargo rust-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup omtose-phellack-theme)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#0E0E0E")
 '(vc-annotate-color-map
   (quote
    ((20 . "#616161")
     (40 . "#9E9E9E")
     (60 . "#9E9E9E")
     (80 . "#C3C3C3")
     (100 . "#C3C3C3")
     (120 . "#DADADA")
     (140 . "#DADADA")
     (160 . "#E8E8E8")
     (180 . "#E8E8E8")
     (200 . "#E8E8E8")
     (220 . "#F1F1F1")
     (240 . "#F1F1F1")
     (260 . "#F1F1F1")
     (280 . "#F6F6F6")
     (300 . "#F6F6F6")
     (320 . "#F6F6F6")
     (340 . "#FAFAFA")
     (360 . "#FAFAFA"))))
 '(vc-annotate-very-old-color "#DADADA"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button))))))
