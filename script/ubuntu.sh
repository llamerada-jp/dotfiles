
set -eux

START_PATH=${PWD}

sudo apt-get -y update
sudo apt-get -y upgrade

# install misc
sudo apt-get -y install build-essential curl git lv valgrind

# set git user
git config --global user.name "Yuji Ito"
git config --global user.email llamerada.jp@gmail.com

# install packages if X-Window-System is enable
if [ -v DISPLAY ]; then
    sudo apt-get -y install dropbox fontforge

    # install fonts
    mkdir -p ~/.fonts

    # install Ricty font
    # if [ ! -e ~/.fonts/Ricty* ]; then
    # 	sudo apt-get -y install fonts-inconsolata fonts-migmix
    # 	curl -L http://www.rs.tus.ac.jp/yyusa/ricty/ricty_generator.sh > /tmp/ricty_generator.sh
    # 	cd /tmp/
    # 	chmod u+x ricty_generator.sh
    # 	./ricty_generator.sh auto
    # 	cp -f Ricty*.ttf ~/.fonts/
    # fi

    # install Myrica font
    if [ ! -e ~/.fonts/Myrica* ]; then
	curl -L https://github.com/tomokuni/Myrica/raw/master/product/Myrica.zip > /tmp/Myrica.zip
	unzip -d /tmp/ /tmp/Myrica.zip
	cp -f /tmp/Myrica.TTC ~/.fonts/
    fi

    fc-cache -vf
    cd ${START_PATH}
fi

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

# install Google Test
sudo apt-get -y install libgtest-dev
cd /usr/src/gtest
sudo cmake .
sudo make
sudo mv /usr/src/gtest/libgtest* /usr/local/lib/

# finishing
sudo apt-get autoclean
