#! /usr/bin/env bash

set -e

echo -e "
... Installing Python and Language Server for Emacs
"

# Python 3.12 dropped distutils which failed elpy
python3_ver=3.11
python_dist_path=$HOME/.local/python${python3_ver}
elpy=${HOME}/.virtualenvs/elpy

# Python
python=${python_dist_path}/bin/python

# Install Python using mamba if not exist
if [ ! -f $python ]; then
    ${HOME}/.local/miniforge3/bin/mamba create --prefix $python_dist_path python=${python3_ver}
fi

pip_index_url=https://pypi.tuna.tsinghua.edu.cn/simple

## Create a virtualenv
${python} -m venv ${elpy}

## Activate and install packages
source ${elpy}/bin/activate
python3 -m pip install pip -U -i ${pip_index_url}
python3 -m pip install 'python-lsp-server[all]' -U -i ${pip_index_url}
python3 -m pip install flake8 jedi autopep8 black -U -i ${pip_index_url}

echo -e "
... Installing Rust and Cargo
"
if [ ! "command -v cargo" ]; then

    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

fi

echo -e "
... Installing ripgrep (https://github.com/BurntSushi/ripgrep)

"
cargo install ripgrep
