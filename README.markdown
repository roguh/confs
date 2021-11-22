# Hugo's Configuration Files

These are my dotfiles. They also include useful scripts, such as a script for setting
the wallpaper in i3 to a host specific file with host specific configuration.

## Downloading

To get started quickly, simply run:

```
cd ~
git clone https://github.com/roguh/confs.git
cd confs
```

If you're going to make changes, you must use SSH to clone it so you can push
directly to the repo. You can also fork this repo and make a pull request to the
main repo.

## Usage

Simply copy the files you need to your home directory.
For example `vim/.vimrc.minimal` would go to `~/vim/.vimrc.minimal`.

Make sure you check to see if a file you want depends on other files.

To backup all supported configuration files:

```
./update.sh backup ..
```

### Advanced

If you know that you want all the configuration files, run

```
./update.sh restore ..
```


## Parallel copying

By default, I run numerous instances of rsync to quickly sync all these small files.

Set `CONFS_COPY_PARALLEL=false` to disable this.

## Submodules

```
git submodule add -repo-
git submodule update --init --recursive
git submodule foreach git pull origin main
```

## OSX

```
brew install coreutils
```

## See what's actually being used

```
./find-used-confs.sh
```
