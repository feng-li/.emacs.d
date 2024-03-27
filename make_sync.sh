#! /usr/bin/bash

set -e

server=$1
rsync -avL ${HOME}/.emacs.d ${server}:
rsync -avL ${HOME}/.config/emacs ${server}:.config/
