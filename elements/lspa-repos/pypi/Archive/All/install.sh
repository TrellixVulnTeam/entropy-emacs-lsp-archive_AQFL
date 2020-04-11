eemacs_lspa_pypi_src="${BASH_SOURCE[0]}"
while [ -h "$eemacs_lspa_pypi_src" ]; do # resolve $eemacs_lspa_pypi_src until the file is no longer a symlink
    eemacs_lspa_pypi_srcdir="$( cd -P "$( dirname "$eemacs_lspa_pypi_src" )" >/dev/null && pwd )"
    eemacs_lspa_pypi_src="$(readlink "$eemacs_lspa_pypi_src")"

    # if $eemacs_lspa_pypi_src was a relative symlink, we need to resolve it relative
    # to the path where the symlink file was located
    [[ $eemacs_lspa_pypi_src != /* ]] && eemacs_lspa_pypi_src="$eemacs_lspa_pypi_srcdir/$eemacs_lspa_pypi_src"
done
eemacs_lspa_pypi_srcdir="$( cd -P "$( dirname "$eemacs_lspa_pypi_src" )" >/dev/null && pwd )"

cd $eemacs_lspa_pypi_srcdir

set -e

[[ ! -d wheels ]] && mkdir wheels
cd wheels/

[[ -d Lib ]] && rm -rf Lib
[[ -d lib ]] && rm -rf lib
[[ -d Script ]] && rm -rf Scripts
[[ -d bin ]] && rm -rf bin

pip install python-language-server[all] grip --prefix $(pwd)

[[ -d Lib ]] && mv -v Lib lib
[[ -d Script ]] && mv -v Script bin
mv lib/*/site-packages lib-tmp \
    && rm -rf lib\
    && mv lib-tmp lib

cd ../

emacs --batch -l eemacs-lspa-pypi-archive-all-bootstrap.el
