@echo off

cd %~dp0

IF NOT EXIST "wheels" ( md wheels )

cd wheels

rmdir /s /q Lib
rmdir /s /q lib
rmdir /s /q Scripts
rmdir /s /q bin

pip install python-language-server[all] --prefix .\

move Lib\site-packages .\lib-tmp
ren lib-tmp lib
ren Scripts bin

cd ..

emacs --batch -l eemacs-lspa-pypi-archive-all-bootstrap.el
