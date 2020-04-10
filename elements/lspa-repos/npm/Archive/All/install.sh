# * code
# ** pre
lspa_npm_soruce="${BASH_SOURCE[0]}"
while [ -h "$lspa_npm_soruce" ]; do # resolve $lspa_npm_soruce until the file is no longer a symlink
    lspa_npm_srcdir="$( cd -P "$( dirname "$lspa_npm_soruce" )" >/dev/null && pwd )"
    lspa_npm_soruce="$(readlink "$lspa_npm_soruce")"

    # if $lspa_npm_soruce was a relative symlink, we need to resolve it relative
    # to the path where the symlink file was located
    [[ $lspa_npm_soruce != /* ]] && lspa_npm_soruce="$lspa_npm_srcdir/$lspa_npm_soruce"
done
lspa_npm_srcdir="$( cd -P "$( dirname "$lspa_npm_soruce" )" >/dev/null && pwd )"

cd $lspa_npm_srcdir

# ** main

npm install tern vscode-html-languageserver-bin vscode-css-languageserver-bin \
    typescript typescript-language-server intelephense bash-language-server\
    vscode-json-languageserver

if [[ ! $? -eq 0 ]]
then
    echo -e "\e[31mNpm install with fatal! Abort.\e[0m"
fi
