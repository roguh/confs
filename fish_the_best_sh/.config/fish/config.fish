# Hugo O. Rivera's FISH config

set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8

function addpaths
  if test -d $argv[1]
    contains -- $argv[1] $fish_user_paths
       or set -U fish_user_paths $fish_user_paths $argv[1]
  end
end


addpaths $HOME/bin
addpaths $HOME/.local/bin
addpaths $HOME/.poetry/bin
addpaths $HOME/.gem/ruby/2.5.0/bin
addpaths $HOME/.gem/ruby/2.6.0/bin
addpaths $HOME/Library/Python/3.7/bin
addpaths $HOME/.dropbox-dist
addpaths $HOME/.cargo/bin
addpaths /opt/flutter/bin

function load_file
    if test -e $argv[1]
        source $argv[1]
    end
end

set -gx ESHELL /bin/bash
set -gx SHELL (which fish)
set -gx EDITOR vim
set -gx VISUAL vim
set -gx REACT_EDITOR none
set -gx PASSWORD_STORE_ENABLE_EXTENSIONS true

if test -d /opt/android-sdk/
  # On Arch, must install aur/android-platform and aur/android-sdk-build-tools
  set -gx ANDROID_SDK_ROOT /opt/android-sdk/
  addpaths $ANDROID_SDK_ROOT/tools
  addpaths $ANDROID_SDK_ROOT/platform-tools
  addpaths $ANDROID_SDK_ROOT/build-tools
end

if type most > /dev/null 2>&1
    set -gx PAGER most
    set -gx pager $PAGER
    set -gx LESS $PAGER
    set -gx MANPAGER $PAGER
    set -gx SYSTEMD_PAGER $PAGER
    alias less=$PAGER
end

# Load aliases
load_file $HOME/.aliases

# Load OCaml
load_file $HOME/.opam/opam-init/init.fish

# mkdir -p ~/.config/fish/completions; and cp ~/.asdf/completions/asdf.fish ~/.config/fish/completions
load_file ~/.asdf/asdf.fish

if type go > /dev/null 2>&1
    set -gx GOPATH (go env GOPATH)
    addpaths $GOPATH/bin
end


# Load pywal theme
# load_theme

function install_plugins
    # "frecency" aware directory switching z
    fisher add jethrokuan/z

    # notifications when commands are done
    fisher add franciscolourenco/done

    # bash like syntax
    # bass export X=4
    fisher add edc/bass
end

# Ruby version manager
# https://rvm.io/rvm/install
# gpg --keyserver hkp://pool.sks-keyservers.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB
# or curl -sSL https://rvm.io/mpapis.asc | gpg --import -
# curl -sSL https://get.rvm.io | bash -s stable --ruby
# curl -L --create-dirs -o ~/.config/fish/functions/rvm.fish https://raw.github.com/lunks/fish-nuggets/master/functions/rvm.fish
if type rvm > /dev/null 2>&1
    rvm default
end

set -gx MANPATH $MANPATH /usr/share/man /usr/local/share/man/

# darwin with some patches
addpaths /usr/local/opt/gettext/bin
addpaths /Library/Frameworks/Python.framework/Versions/3.7/bin
addpaths /usr/local/opt/openssl@1.1/bin
addpaths /usr/local/opt/openssl/bin

if test -d /usr/local/opt/openssl/
  set -gx LDFLAGS "-L/usr/local/opt/openssl/lib"
  set -gx CPPFLAGS "-I/usr/local/opt/openssl/include"
  set -gx PKG_CONFIG_PATH "/usr/local/opt/openssl/lib/pkgconfig"
end

if test -d /usr/local/opt/gettext/lib
  set -gx LDFLAGS "-L/usr/local/opt/gettext/lib"
  set -gx CPPFLAGS "-I/usr/local/opt/gettext/include"
end

if test -d /usr/local/opt/openssl@1.1/lib
  set -gx LDFLAGS "-L/usr/local/opt/openssl@1.1/lib"
  set -gx CPPFLAGS "-I/usr/local/opt/openssl@1.1/include"
  set -gx PKG_CONFIG_PATH "/usr/local/opt/openssl@1.1/lib/pkgconfig"
end


# Have fzf use ag to find files
if type ag > /dev/null 2>&1
    set -gx FZF_DEFAULT_COMMAND 'ag -g ""'
    set -gx FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"
end

function ,,
  echo (projectroot.sh)
  cd (projectroot.sh)
end

if status is-interactive
    xset r rate 200 60

    if not functions -q fisher
        set -qx XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
        curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
        fish -c fisher
    end

    function fish_user_key_bindings
        # Use Ctrl-R to find command in history
        fzf_key_bindings

        # Use Ctrl-P to find files
        bind \cp fzf-file-widget

        if bind -M insert > /dev/null 2>&1 2>&1
            bind -M insert \cp fzf-file-widget
        end

        # Ctrl-F is essential fish
        # It can become unbound, e.g. if in vi-mode
        # Right Arrow and Ctrl-E might work
        bind \cf forward-char
    end

    if type keychain > /dev/null 2>&1
        eval (keychain --eval --agents ssh -Q --quiet --nogui id_ed25519 id_ed25519_2) &
    end
end

# Fish does lots of things by default:
# ignore dups and blank lines in history
# interactive cd and autocompletion
