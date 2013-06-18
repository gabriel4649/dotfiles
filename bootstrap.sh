#!/usr/bin/env bash
#cd "$(dirname "${BASH_SOURCE}")"
#git pull origin master
function doIt() {
    rsync --exclude ".git/" --exclude "README.org" --exclude "bootstrap.sh" \
        --exclude "packages.list" --exclude "LICENSE-GPL.txt" \
        --exclude "cleanstart-packages.list.sh" -av --no-perms . ~
}
if [ "$1" == "--force" -o "$1" == "-f" ]; then
    doIt
else
    read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        doIt
    fi
fi
unset doIt
#source ~/.bash_profile
