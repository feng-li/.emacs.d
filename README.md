# Copyright 
 
See each individual file.

# Install

* Back up old files

```
cd ~
mv .emacs.d .emacs.d-bak
mv .emacs .emacs-bak
```

* Clone the repository

```
cd ~
git clone https://feng-li@github.com/feng-li/.emacs.d.git
ln -sf .emacs.d/.emacs
cd ~/.emacs.d/
git submodule update --init --recursive
```

# Synchronize with the remote

```
cd ~/.emacs.d/
git pull
git submodule sync 
git submodule foreach git checkout master
git submodule foreach git pull
git submodule update
```