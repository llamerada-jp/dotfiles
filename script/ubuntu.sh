
set -eux

START_PATH=${PWD}

sudo apt-get -y update
sudo apt-get -y upgrade

# install misc
sudo apt-get -y install build-essential curl git lv valgrind dropbox

# set git user
git config --global user.name "Yuji Ito"
git config --global user.email llamerada.jp@gmail.com

# install packages if X-Window-System is enable
if [ -v DISPLAY ]; then
    sudo apt-get -y install fontforge

    # install fonts
    mkdir -p ~/.fonts

    # install Myrica font
    if [ ! -e ~/.fonts/Myrica* ]; then
	curl -L https://github.com/tomokuni/Myrica/raw/master/product/Myrica.zip > /tmp/Myrica.zip
	unzip -d /tmp/ /tmp/Myrica.zip
	cp -f /tmp/Myrica.TTC ~/.fonts/
    fi

    fc-cache -vf
    cd ${START_PATH}
fi

# install oracle java
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java8-installer

# install emacs
if [ -v DISPLAY ]; then
    chkpkg=$(apt-cache pkgnames emacs | wc -l)
    chkpkg24=$(apt-cache pkgnames emacs24 | wc -l)
    if [ ${chkpkg} -ne 0 ]; then
	sudo apt-get install -y emacs
    elif [ ${chkpkg} -ne 0 ]; then
	sudo apt-get install -y emacs24
    fi
else
    chkpkg=$(apt-cache pkgnames emacs-nox | wc -l)
    chkpkg24=$(apt-cache pkgnames emacs24-nox | wc -l)
    if [ ${chkpkg} -ne 0 ]; then
	sudo apt-get install -y emacs-nox
    elif [ ${chkpkg} -ne 0 ]; then
	sudo apt-get install -y emacs24-nox
    fi
fi

if [ ! -e ~/.cask ]; then
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

mkdir -p ~/.emacs.d

ln -fs `pwd`/emacs/init.el ~/.emacs.d/
ln -fs `pwd`/emacs/Cask ~/.emacs.d/

cd ~/.emacs.d
~/.cask/bin/cask upgrade-cask
~/.cask/bin/cask upgrade
~/.cask/bin/cask install
~/.cask/bin/cask update

cd ${START_PATH}

# install cpplint
curl https://raw.githubusercontent.com/google/styleguide/gh-pages/cpplint/cpplint.py > /tmp/cpplint.py
sudo mv /tmp/cpplint.py /usr/local/bin/
sudo chmod 755 /usr/local/bin/cpplint.py

# finishing
sudo apt-get autoclean
