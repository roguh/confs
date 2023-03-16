# Hugo O. Rivera's FISH config
# Remember to run `install_plugins` once.

# localectl set-locale LANG=fr_FR.UTF-8 LANGUAGE=fr_FR.UTF-8:es_US.UTF-8:en_US.UTF-8:C LC_COLLATE=C

set START_TIME (date +%s.%N)
set FAST_STARTUP true
set DEBUG_OUTPUT false

# if [ "$PWD" = "$HOME" ] && status is-interactive
#   set DEBUG_OUTPUT true
# end

function log
  set_color -i
  echo "$FISH_LOGO $argv"
  set_color normal
end

function debug
  if [ "$DEBUG_OUTPUT" = true ]
    log "$argv"
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

debug Starting FISH debug output. Set DO_NOT_CLEAR to leave it on the screen after startup.
set_global DO_NOT_CLEAR true

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
addpaths /opt/cuda/bin
addpaths /opt/asdf-vm/bin/

# set_global_if_unset LC_ALL en_US.UTF-8
# set_global_if_unset LANG en_US.UTF-8

set_global_if_unset ESHELL /bin/bash
set_global_if_unset SHELL (which fish)
set_global_if_unset EDITOR vim
set_global_if_unset VISUAL vim
set_global REACT_EDITOR none
set_global PASSWORD_STORE_ENABLE_EXTENSIONS true
set_global PYTHON_KEYRING_BACKEND keyring.backends.null.Keyring

# This is used for speeding up integration/unit tests on a private repo
set_global TEST_TIMEOUT_SCALING_FACTOR 2

# To disable parallely notifications unless a failure happens
set_global_if_unset NOTIFY_COMMAND 'true'

set_global USE_GKE_GCLOUD_AUTH_PLUGIN True

set_global_if_unset PYTHONSTARTUP "$HOME/.ipython/profile_default/startup/10-imports.py"

if test -d /opt/android-sdk/
  # On Arch, must install aur/android-platform and aur/android-sdk-build-tools
  set_global ANDROID_SDK_ROOT /opt/android-sdk/
  addpaths $ANDROID_SDK_ROOT/tools
  addpaths $ANDROID_SDK_ROOT/platform-tools
  addpaths $ANDROID_SDK_ROOT/build-tools

  debug Set Android variables and paths
end

if command -v most > /dev/null 2>&1
    set_global PAGER most
    set_global pager $PAGER
    set_global LESS $PAGER
    set_global MANPAGER $PAGER
    set_global SYSTEMD_PAGER $PAGER
    alias less=$PAGER
end

if command -v git > /dev/null
    abbr ga 'git add'
    abbr gc 'git commit'
    abbr gch 'git checkout'
    abbr gchm 'git checkout main || git checkout master || git checkout trunk'
    abbr gs 'git status'
    abbr gst 'git stash push --'
    abbr gstp 'git stash pop'
    abbr gd 'git diff'
    abbr gdt 'git difftool'
    abbr gl 'git log'
    abbr gcl 'git clone'
    debug Setup Git abbreviations
end

if command -v kubectl > /dev/null
    abbr k kubectl
    abbr kx kubectx
    abbr kc 'kubectl config'
    # List and detail resources
    abbr kg 'kubectl get'
    abbr kgp 'kubectl get pods'
    abbr kgs 'kubectl get services'
    abbr kga 'kubectl get applications'
    abbr kd 'kubectl describe'
    abbr kdl 'kubectl delete'
    # Debugging pods
    abbr kl 'kubectl logs'
    abbr kcp 'kubectl cp'
    abbr kex 'kubectl exec'
    abbr kpf 'kubectl port-forward'
    abbr kubectl-stop-sync-app 'kubectl -n argocd patch --type=merge application -p "{\"spec\":{\"syncPolicy\":null}}"'
    abbr kubectl-start-sync-app 'kubectl -n argocd patch --type=merge application -p "{\"spec\":{\"syncPolicy\":{\"automated\":{\"selfHeal\":true}}}}"'

    debug Setup Kubernetes abbreviations

    addpaths $HOME/.krew/bin
end

if command -v docker > /dev/null
    abbr dcls 'docker container ls'
    abbr dl 'docker logs'
    abbr dex 'docker exec'
    abbr dck 'docker container kill (docker ps -q)'
    debug Setup Docker abbreviations
end

if command -v makeanywhere > /dev/null
    set -g MAKEANYWHERE (which makeanywhere)
    function makeanywhere --wraps make --description "makeanywhere --wraps make $MAKEANYWHERE"
        "$MAKEANYWHERE" $argv
    end
    
    function pma --wraps make --description "pma --wraps make pipenv run $MAKEANYWHERE"
        pipenv run "$MAKEANYWHERE" $argv
    end

    alias ma makeanywhere

    debug Setup makeanywhere alias
end

function pmake --wraps make --description "pma --wraps make pipenv run"
    pipenv run make $argv
end

# Load aliases
# TODO reduce number of aliases to speed up loading time or create ~/.essential_aliases
# I tried getting rid of tryalias with "alias tryalias alias", but it's actually
# almost as fast as plain alias so I'll keep it.
load_file $HOME/.aliases --verbose
tryalias ,, commacomma

# Load OCaml
load_file $HOME/.opam/opam-init/init.fish

load_file ~/.asdf/asdf.fish --verbose
load_file /opt/asdf-vm/asdf.fish --verbose

if test -e ~/.asdf/completions/asdf.fish
  mkdir -p ~/.config/fish/completions
  rm -f ~/.config/fish/completions/asdf.fish
  cp ~/.asdf/completions/asdf.fish ~/.config/fish/completions
  debug Loaded ASDF fish completions
end

if command -v go > /dev/null 2>&1
  if [ "$FAST_STARTUP" ]
    debug SPEEDUP: Guessing GOPATH
    set_global GOPATH "$HOME/go"
  else
    set_global GOPATH (go env GOPATH)
  end
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
if command -v rvm > /dev/null 2>&1
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
if command -v ag > /dev/null 2>&1
  if command -v fzf > /dev/null 2>&1
    set_global FZF_DEFAULT_COMMAND 'ag --hidden -g ""'
    set_global FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"

    debug Set fzf variables
  end
end

function anaconda_fish_init
  eval "$HOME/anaconda3/bin/conda" "shell.fish" "hook" $argv | source
end

function miniconda_fish_init
  # set --local CONDA_BIN "$HOME/miniconda3/bin/conda"
  set --local CONDA_BIN "/opt/miniconda3/bin/conda"
  if ! command -v "$CONDA_BIN" > /dev/null
    return 1
  end
  eval "$CONDA_BIN" "shell.fish" "hook" $argv | source
  conda activate torch
  debug Loaded miniconda and its torch environment
end

# If this is overwriting your system's Python:
# conda config --set auto_activate_base false
#
# miniconda_fish_init

if status is-interactive
  # Load direnv
  if command -v direnv > /dev/null 2>&1
    direnv hook fish | source
    debug Loaded direnv
  end

  if command -v xset > /dev/null 2>&1 && [ -n "$DISPLAY" ]
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
    if command -v fzf > /dev/null 2>&1
      # Use Ctrl-R to find command in history
      fzf_key_bindings

      # Use Ctrl-P to find files
      bind \cp fzf-file-widget

      if bind -M insert > /dev/null 2>&1 2>&1
        bind -M insert \cp fzf-file-widget
      end
      # debug Configured interactive fzf features
    end

    # Ctrl-F is essential fish
    # It can become unbound, e.g. if in vi-mode
    # Right Arrow and Ctrl-E might work
    bind \cf forward-char
    # debug Bound Ctrl-F
  end

  if command -v keychain > /dev/null 2>&1
    if [ "$FAST_STARTUP" = true ] && [ "$SSH_AGENT_PID" != "" ] && [ -e "/proc/$SSH_AGENT_PID/status" ]
        debug SPEEDUP Skipping calling keychain as SSH_AGENT_PID is already set and ssh-agent is running
    else
        eval (keychain --eval --agents ssh -Q --quiet --nogui id_ed25519) &
        debug Started ssh-agent with keychain
        if ! [ -e "/proc/$SSH_AGENT_PID/status" ]
          if [ (ps "$SSH_AGENT_PID" | wc -l) -ge 2 ]
            warn Your system does not support procfs 
          else
            warn The SSH agent was not started!
          end
        end
    end
  end

  # Waiting for https://github.com/starship/starship/issues/3305 to be fixed
  # Applying temp fix manually
  # starship init fish | source
  if command -v starship > /dev/null
    load_file ~/.config/fish/fish_starship_prompt.fish
  end

  set TOTAL_STARTUP_TIME (echo (date +%s.%N) "$START_TIME" | awk '{print ($1 - $2) * 1000}' || echo UNKNOWN)
  log "$TOTAL_STARTUP_TIME"ms
  echo "$TOTAL_STARTUP_TIME" (date +%Y-%m-%d) >> "$HOME/tmp/fish_startup_times"
end

set SHELL_TYPE ([ -n "$SSH_CLIENT" ] && echo ' SSH' || echo)
switch $hostname$SHELL_TYPE
  case 'raspberrypi' '*SSH*'
    if [ "$hostname" = raspberrypi ]
      set HOSTNAME_SUMMARY "a Raspberry Pi"
    else
      set HOSTNAME_SUMMARY "SSH on $hostname (SSH_CLIENT=$SSH_CLIENT)"
    end
    set USER_AND_HOST_COLOR brred
  case '*T580*' localhost
    set HOSTNAME_SUMMARY "a known host"
    set USER_AND_HOST_COLOR brcyan
  case '*flex*'
    set HOSTNAME_SUMMARY "a known host"
    set USER_AND_HOST_COLOR bryellow
  case '2012-iMac'
    set HOSTNAME_SUMMARY "a known host"
    set USER_AND_HOST_COLOR bryellow
  case '*'
    set HOSTNAME_SUMMARY "an UNKNOWN host"
    set USER_AND_HOST_COLOR brwhite
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
  if command -v kubectl > /dev/null 2>&1
    fisher install evanlucas/fish-kubectl-completions
  end

  # pyenv
  if command -v pyenv > /dev/null 2>&1
    fisher install oh-my-fish/plugin-pyenv
  end
end

# Fish does lots of things by default:
# ignore dups and blank lines in history
# interactive cd and autocompletion

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/fish/__tabtab.fish ]; and . ~/.config/tabtab/fish/__tabtab.fish; or true
