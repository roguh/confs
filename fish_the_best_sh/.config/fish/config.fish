# Hugo O. Rivera's FISH config
# Remember to run `install_plugins` once.

set FAST_STARTUP true
set DEBUG_OUTPUT false

if [ "$PWD" = "$HOME" ] && status is-interactive
  set DEBUG_OUTPUT true
end

function debug
  if [ "$DEBUG_OUTPUT" = true ]
    set_color -i
    echo "$FISH_LOGO $argv"
    set_color normal
  end
end

function warn
  set_color bryellow black
  debug WARNING: $argv
end

function addpaths --argument-names 'path' 'verbose'
  if test -d "$path"
    if not contains -- "$path" $fish_user_paths
      # Must check if path is already added.
      # Without this check, fish becomes gradually slower to start as it
      # struggles to manage an enormous variable.
      set -U fish_user_paths $fish_user_paths "$path"
      debug Added path (trimdir.py "$path")
    end
  else if ! [ "$verbose" = "" ]
    warn addpaths could not find $argv[1]
  end
end

function load_file --argument-names 'file' 'verbose'
    if test -e $file
        source $file
        debug Loaded file $file
    else if ! [ "$verbose" = "" ]
        warn File not found: $file
    end
end

function set_global
  set -gx $argv
  debug Set variable $argv[1]
end

function set_global_if_unset
  if not set -q $argv[1]
    set_global $argv
  end
end

load_file $HOME/.config/fish/local_env.fish
set_global_if_unset FISH_LOGO Fish # 🐠
set_global DOCKER_BUILDKIT 1

addpaths $HOME/bin --verbose
addpaths $HOME/.local/bin  --verbose
addpaths $HOME/.poetry/bin
addpaths $HOME/.gem/ruby/2.5.0/bin
addpaths $HOME/.gem/ruby/2.6.0/bin
addpaths $HOME/Library/Python/3.7/bin
addpaths $HOME/.dropbox-dist  --verbose
addpaths $HOME/.cargo/bin
addpaths $HOME/.pyenv/bin
addpaths /opt/flutter/bin

set_global_if_unset LC_ALL en_US.UTF-8
set_global_if_unset LANG en_US.UTF-8

set_global_if_unset ESHELL /bin/bash
set_global_if_unset SHELL (which fish)
set_global_if_unset EDITOR vim
set_global_if_unset VISUAL vim
set_global REACT_EDITOR none
set_global PASSWORD_STORE_ENABLE_EXTENSIONS true

if test -d /opt/android-sdk/
  # On Arch, must install aur/android-platform and aur/android-sdk-build-tools
  set_global ANDROID_SDK_ROOT /opt/android-sdk/
  addpaths $ANDROID_SDK_ROOT/tools
  addpaths $ANDROID_SDK_ROOT/platform-tools
  addpaths $ANDROID_SDK_ROOT/build-tools

  debug Set Android variables and paths
end

if type most > /dev/null 2>&1
    set_global PAGER most
    set_global pager $PAGER
    set_global LESS $PAGER
    set_global MANPAGER $PAGER
    set_global SYSTEMD_PAGER $PAGER
    alias less=$PAGER
end

load_file $HOME/.config/fish/abbrs.fish --verbose

# Load aliases
# TODO reduce number of aliases to speed up loading time or create ~/.essential_aliases
load_file $HOME/.aliases --verbose
tryalias ,, commacomma

# Load OCaml
load_file $HOME/.opam/opam-init/init.fish

# mkdir -p ~/.config/fish/completions; and cp ~/.asdf/completions/asdf.fish ~/.config/fish/completions
load_file ~/.asdf/asdf.fish --verbose

if command -v yarn 2>&1 > /dev/null
    if [ "$FAST_STARTUP" = true ]
        debug SPEEDUP: Reading ~/.tool-versions to find NPM path instead of calling yarn
        set TOOL_VERSIONS_FILE "$HOME/.tool-versions"
        if [ -f "$TOOL_VERSIONS_FILE" ] && grep nodejs "$TOOL_VERSIONS_FILE" &>/dev/null
            set NODE_VERSION (grep nodejs "$TOOL_VERSIONS_FILE" | cut -f2 -d' ')
            addpaths "$HOME/.asdf/installs/nodejs/$NODE_VERSION/.npm/bin" --verbose
        else
            warn "ASDF installation of NodeJS not found."
        end
    else
        addpaths (yarn global bin) --verbose
    end
end

if type go > /dev/null 2>&1
  set_global GOPATH (go env GOPATH)
  addpaths $GOPATH/bin --verbose
  debug Set Go variables and paths
end


# Load pywal theme
# load_theme

# Ruby version manager
# https://rvm.io/rvm/install
# gpg --keyserver hkp://pool.sks-keyservers.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB
# or curl -sSL https://rvm.io/mpapis.asc | gpg --import -
# curl -sSL https://get.rvm.io | bash -s stable --ruby
# curl -L --create-dirs -o ~/.config/fish/functions/rvm.fish https://raw.github.com/lunks/fish-nuggets/master/functions/rvm.fish
if type rvm > /dev/null 2>&1
  rvm default
  debug Loaded RVM
end

set_global MANPATH $MANPATH /usr/share/man /usr/local/share/man/

# darwin with some patches
addpaths /usr/local/opt/gettext/bin
addpaths /Library/Frameworks/Python.framework/Versions/3.7/bin
addpaths /usr/local/opt/openssl@1.1/bin
addpaths /usr/local/opt/openssl/bin

if test -d /usr/local/opt/openssl/
  set_global LDFLAGS "-L/usr/local/opt/openssl/lib"
  set_global CPPFLAGS "-I/usr/local/opt/openssl/include"
  set_global PKG_CONFIG_PATH "/usr/local/opt/openssl/lib/pkgconfig"

  debug Set local openssl variables
end

if test -d /usr/local/opt/gettext/lib
  set_global LDFLAGS "-L/usr/local/opt/gettext/lib"
  set_global CPPFLAGS "-I/usr/local/opt/gettext/include"

  debug Set local gettext variables
end

if test -d /usr/local/opt/openssl@1.1/lib
  set_global LDFLAGS "-L/usr/local/opt/openssl@1.1/lib"
  set_global CPPFLAGS "-I/usr/local/opt/openssl@1.1/include"
  set_global PKG_CONFIG_PATH "/usr/local/opt/openssl@1.1/lib/pkgconfig"

  debug Set local openssl@1.1 variables
end


# Have fzf use ag to find files
if type ag > /dev/null 2>&1
  if type fzf > /dev/null 2>&1
    set_global FZF_DEFAULT_COMMAND 'ag --hidden -g ""'
    set_global FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"

    debug Set fzf variables
  end
end

if status is-interactive
  if type xset > /dev/null 2>&1
    xset r rate 200 60
    debug Set keyboard rate
  end

  if command -v gh > /dev/null
    if [ "$FAST_STARTUP" = true ]
      debug SPEEDUP Skipping GitHub gh completions
    else
      eval (gh completion --shell fish)
      debug Added GitHub gh completions
    end
  end

  function fish_user_key_bindings
    if type fzf > /dev/null 2>&1
      # Use Ctrl-R to find command in history
      fzf_key_bindings

      # Use Ctrl-P to find files
      bind \cp fzf-file-widget

      if bind -M insert > /dev/null 2>&1 2>&1
        bind -M insert \cp fzf-file-widget
      end
      debug Configured interactive fzf features
    end

    # Ctrl-F is essential fish
    # It can become unbound, e.g. if in vi-mode
    # Right Arrow and Ctrl-E might work
    bind \cf forward-char
    debug Bound Ctrl-F
  end

  if type keychain > /dev/null 2>&1
    if [ "$FAST_STARTUP" = true ] && [ "$SSH_AGENT_PID" != "" ]
        debug SPEEDUP Skipping calling keychain as SSH_AGENT_PID is already set
    else
        eval (keychain --eval --agents ssh -Q --quiet --nogui id_ed25519) &
        debug Loaded keychain
    end
  end
end

function install_plugin_manager
  curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
end

function install_plugins
  # Run bash commands
  fisher install edc/bass

  # Done
  fisher install franciscolourenco/done

  # kubectl completion
  if type kubectl > /dev/null 2>&1
    fisher install evanlucas/fish-kubectl-completions
  end

  # pyenv
  if type pyenv > /dev/null 2>&1
    fisher install oh-my-fish/plugin-pyenv
  end
end

# Fish does lots of things by default:
# ignore dups and blank lines in history
# interactive cd and autocompletion
