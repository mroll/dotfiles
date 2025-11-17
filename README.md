# Dotfiles

Personal configuration files managed with GNU Stow.

## Contents

- **emacs/** - Emacs configuration with HFT research workflow
- **doom/** - Old Doom Emacs config (archived)
- **spacemacs/** - Old Spacemacs config (archived)
- **msmtp/** - Mail configuration
- **news/** - News reader configuration
- **offlineimap/** - Email sync configuration

## Emacs Configuration

The Emacs config lives in `emacs/.config/emacs/` and is now organized into self-contained modules so each concern can be reasoned about independently.

### Modular Layout

```
~/.config/emacs/
├── init.el          # Loader: sets load-paths, custom-file, and module order
├── var/             # Generated files (custom.el, caches)
└── lisp/
    ├── core/        # Foundation (settings, bootstrap, editing, keybinds)
    │   ├── my-core-settings.el
    │   ├── my-core-packages.el
    │   ├── my-core-editing.el
    │   └── my-core-keybinds.el
    ├── modules/     # Feature slices (completion, Evil, Org, notes, etc.)
    │   ├── my-module-completion.el
    │   ├── my-module-evil.el
    │   ├── my-module-ui.el
    │   ├── my-module-org.el
    │   ├── my-module-org-roam.el
    │   ├── my-module-projects.el
    │   ├── my-module-notes.el
    │   └── my-module-navigation.el
    └── lang/        # Language/tooling integrations
        ├── my-lang-lsp.el
        ├── my-lang-c.el
        ├── my-lang-web.el
        ├── my-lang-ocaml.el
        ├── my-lang-python.el
        └── my-lang-tex.el
```

Modules are loaded in `init.el` via the ordered `my/emacs-modules` list. To disable a feature, remove its symbol from that list; to add a new module, drop an `my-module-*.el` or `my-lang-*.el` file under `lisp/` (each file must end with a `(provide 'feature-name)` form) and then append the feature to `my/emacs-modules`.

Run a batch validation after changes:

```bash
emacs --batch -l ~/.config/emacs/init.el --eval "(message \"ok\")"
```

If you use `org-roam`/`citar-org-roam`, make sure Emacs has SQLite support (built-in on Emacs ≥29 if compiled with `--with-sqlite3`) or install the `sqlite3` Elisp package plus your system’s `libsqlite3` shared library; without it, the loader will log warnings and abort those modules.

The original HFT workflow still lives in `hft-research.el`, which now consumes the shared modules instead of defining packages inline.

### Core Features
- **Package Management**: straight.el for reproducible package management
- **Evil Mode**: Vim keybindings with Space as leader key
- **Completion**: vertico + consult + marginalia + embark + orderless
- **Note Taking**: Denote (filename-based) + org-roam for knowledge management
- **Bibliography**: Citar for BibTeX management
- **Languages**: OCaml (tuareg, merlin, dune, utop), Python, C/C++ (lsp-mode)

### HFT Research Workflow

Specialized configuration for High-Frequency Trading research in `hft-research.el`:

- **Note System**: Organized in `~/notes/hft/` with subdirectories for concepts, sources, experiments, lab notebooks
- **Bibliography**: Integrated with `~/notes/hft/hft.bib`
- **Templates**: Pre-configured templates for source notes, concept notes, experiments, and questions
- **Org-Babel**: Python and OCaml code blocks for inline experiments

### Keybindings (Space Leader)

**Notes (`SPC n`)**
- `SPC n q` - Quick question capture
- `SPC n c` - New concept note
- `SPC n e` - New experiment/lab note
- `SPC n l` - Insert denote link
- `SPC n b` - Show backlinks
- `SPC n r` - Rename note

**Search (`SPC s`)**
- `SPC s n` - Search all HFT notes
- `SPC s N` - Search concepts only
- `SPC s x` - Search experiments only

**Files (`SPC f`)**
- `SPC f n` - Find note
- `SPC f N` - Denote find/create

**References (`SPC r`)**
- `SPC r r` - Open bibliography
- `SPC r i` - Insert citation
- `SPC r o` - Open PDF
- `SPC r n` - Create source note from citation

**Org-Mode Local (`,` prefix)**
- `, c c` - Org-capture (`,` acts as a prefix; `, c I` / `, c O` handle clocking)
- `, l l` - Insert denote/org-roam link
- `, l c` - Link to concept
- `, l b` - Show backlinks
- `, c i` - Insert citation (from HFT module)
- `, c o` - Open citation PDF (from HFT module)
- `, x` - Execute code block

See `~/notes/hft/README.md` for full workflow documentation.

## Setup

### Prerequisites

Install dependencies:
```bash
brew install stow
brew install poppler  # For pdf-tools
```

### Installation

1. Clone this repository:
```bash
git clone <repo-url> ~/dotfiles
cd ~/dotfiles
```

2. Backup existing configs (if any):
```bash
mv ~/.config/emacs ~/.config/emacs.backup
```

3. Stow the Emacs config:
```bash
stow -v -d ~/dotfiles -t ~ emacs
```

This will create a symlink: `~/.config/emacs -> ~/dotfiles/emacs/.config/emacs`

4. Create notes directory:
```bash
mkdir -p ~/notes/hft/{inbox,concepts,sources,projects,lab,glossary,pdfs}
```

5. Start Emacs:
```bash
emacs
```

On first launch, straight.el will automatically install all packages. This may take a few minutes.

6. Build pdf-tools (if needed):
```
M-x pdf-tools-install
```

## Updating Configuration

Since the config is symlinked via Stow, you can edit files directly:

```bash
# Edit the config
vim ~/.config/emacs/init.el

# Changes are automatically reflected in the dotfiles repo
cd ~/dotfiles
git status  # Will show changes to emacs/.config/emacs/init.el

# Commit changes
git add emacs/.config/emacs/init.el
git commit -m "Update emacs config"
```

## Bootstrap on New Machine

```bash
# Install dependencies
brew install stow poppler

# Clone dotfiles
git clone <repo-url> ~/dotfiles

# Stow configs
cd ~/dotfiles
stow emacs

# Create notes directory
mkdir -p ~/notes/hft/{inbox,concepts,sources,projects,lab,glossary,pdfs}

# Launch Emacs (packages will auto-install)
emacs
```

## Structure

```
dotfiles/
├── emacs/
│   ├── .config/
│   │   └── emacs/
│   │       ├── init.el              # Loader + module list
│   │       ├── early-init.el        # Early initialization
│   │       ├── hft-research.el      # HFT research workflow
│   │       └── lisp/                # Core/modules/lang structure (see above)
│   ├── .stow-local-ignore           # Stow ignore patterns
│   ├── lisp/                        # Custom lisp packages
│   └── archive/                     # Old configs
├── doom/                            # Old Doom config
├── spacemacs/                       # Old Spacemacs config
└── README.md                        # This file
```

## Notes Directory Structure

```
~/notes/hft/
├── inbox/          # Quick captures, questions
├── concepts/       # Models, theorems, techniques
├── sources/        # Books, papers (one note per source)
├── projects/       # Specific investigations
├── lab/            # Experiment notebooks
├── glossary/       # Short definitions
├── pdfs/           # PDF storage (by bibkey)
├── hft.bib         # Bibliography file
└── README.md       # Workflow documentation
```

## Troubleshooting

### Packages not installing
```elisp
M-x straight-pull-all
M-x straight-rebuild-all
```

### Org-roam / citar-org-roam errors about SQLite

Make sure Emacs has SQLite support. On macOS with Homebrew:

```bash
brew install sqlite
```

Then install the Emacs `sqlite3` package (e.g., `M-x package-install RET sqlite3 RET`) if your Emacs build lacks native SQLite. Restart Emacs and rerun `emacs --batch -l ~/.config/emacs/init.el`.

### PDF-tools not working
```bash
brew install poppler automake
```
Then in Emacs:
```elisp
M-x pdf-tools-install
```

### Symlinks broken after Stow
```bash
# Remove broken symlinks
stow -D emacs

# Re-stow
stow -v emacs
```

## License

Personal configuration files. Use at your own risk.
