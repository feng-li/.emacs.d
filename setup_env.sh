#! /usr/bin/env bash

# Python 3.12 dropped distutils which failed elpy
python=python3.11

elpy=${HOME}/.virtualenvs/elpy
pip_index_url=https://pypi.tuna.tsinghua.edu.cn/simple

## Create a virtualenv
virtualenv --python ${python} ${elpy} --clear

## Activate and install packages
source ${elpy}/bin/activate
python3 -m pip install pip -U -i ${pip_index_url}
python3 -m pip install 'python-lsp-server[all]' -U -i ${pip_index_url}
python3 -m pip install flake8 jedi autopep8 black -U -i ${pip_index_url}


echo -e "
... Installing Rust and Cargo
"
if [ ! "command -v cargo" ]; then

curl https://sh.rustup.rs -sSf | sh

fi

echo -e "
... Installing ripgrep (https://github.com/BurntSushi/ripgrep)

"
cargo install ripgrep
