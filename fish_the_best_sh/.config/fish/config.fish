# Hugo O. Rivera's FISH config

set DEBUG_OUTPUT = false

if [ "$PWD" = "$HOME" ]
  if status is-interactive
    set DEBUG_OUTPUT true
  end
end

function debug
  if [ "$DEBUG_OUTPUT" = true ]
    echo $FISH_LOGO: $argv
  end
end

function addpaths
  if test -d $argv[1]
    if not contains -- $argv[1] $fish_user_paths
      # Must check if path is already added.
      # Without this check, fish becomes gradually slower to start as it
      # struggles to manage an enormous variable.
      set -U fish_user_paths $fish_user_paths $argv[1]
      debug Added path (trimdir.py $argv[1])
    end
  end
end

function load_file
    if test -e $argv[1]
        source $argv[1]
        debug Loaded file $argv[1]
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

addpaths $HOME/bin
addpaths $HOME/.local/bin
addpaths $HOME/.poetry/bin
addpaths $HOME/.gem/ruby/2.5.0/bin
addpaths $HOME/.gem/ruby/2.6.0/bin
addpaths $HOME/Library/Python/3.7/bin
addpaths $HOME/.dropbox-dist
addpaths $HOME/.cargo/bin
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

# Load aliases
load_file $HOME/.aliases
tryalias ,, commacomma

# Load OCaml
load_file $HOME/.opam/opam-init/init.fish

# mkdir -p ~/.config/fish/completions; and cp ~/.asdf/completions/asdf.fish ~/.config/fish/completions
load_file ~/.asdf/asdf.fish

if type go > /dev/null 2>&1
  set_global GOPATH (go env GOPATH)
  addpaths $GOPATH/bin
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
    set_global FZF_DEFAULT_COMMAND 'ag -g ""'
    set_global FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"

    debug Set fzf variables
  end
end

if status is-interactive
  if type xset > /dev/null 2>&1
    xset r rate 200 60
    debug Set keyboard rate
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
    eval (keychain --eval --agents ssh -Q --quiet --nogui id_ed25519) &
    debug Loaded keychain
  end
end

# Fish does lots of things by default:
# ignore dups and blank lines in history
# interactive cd and autocompletion
