#! /usr/bin/env bash

python=python3.11
elpy=${HOME}/.virtualenvs/elpy
pip_index_url=https://pypi.tuna.tsinghua.edu.cn/simple

## Create a virtualenv
virtualenv --python ${python} ${elpy} --always-copy --clear

## Activate and install packages
source ${elpy}/bin/activate
python3 -m pip install pip -U -i ${pip_index_url}
python3 -m pip install 'python-lsp-server[all]' -U -i ${pip_index_url}
python3 -m pip install flake8 jedi autopep8 black -U -i ${pip_index_url}
