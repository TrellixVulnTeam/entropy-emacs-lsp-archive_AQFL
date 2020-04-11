@echo off

cd %~dp0

IF NOT EXIST "wheels" ( md wheels )

cd wheels

IF EXIST "Lib\" ( rmdir /s /q Lib )
IF EXIST "lib\" ( rmdir /s /q lib )
IF EXIST "Scripts\" ( rmdir /s /q Scripts )
IF EXIST "bin\" ( rmdir /s /q bin )

pip install python-language-server[all] --prefix .\

move Lib\site-packages .\lib-tmp
rmdir /s /q Lib
ren lib-tmp lib
ren Scripts bin

cd ..

emacs --batch -l eemacs-lspa-pypi-archive-all-bootstrap.el
