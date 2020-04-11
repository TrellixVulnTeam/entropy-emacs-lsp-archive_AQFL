@echo off

cd %~dp0

npm install tern vscode-html-languageserver-bin vscode-css-languageserver-bin ^
    typescript typescript-language-server intelephense bash-language-server ^
    vscode-json-languageserver
