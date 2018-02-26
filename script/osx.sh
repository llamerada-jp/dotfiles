
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
START_PATH=${PWD}

brew update
brew upgrade

brew install pkg-config automake cask lv node socat socat
brew cask install xquartz

# set git user
git config --global user.name "Yuji Ito"
git config --global user.email llamerada.jp@gmail.com

# install Myrica font
if [ ! -e ~/.fonts/Myrica* ]; then
    curl -L https://github.com/tomokuni/Myrica/raw/master/product/Myrica.zip > /tmp/Myrica.zip
    unzip -d /tmp/ /tmp/Myrica.zip
    cp -f /tmp/Myrica.TTC ~/Library/Fonts/
fi

fc-cache -vf

# install emacs
brew install --with-cocoa --with-librsvg --with-gnutls emacs
brew cask install emacs

mkdir ~/.emacs.d/themes
mkdir ~/Develop

ln -s `pwd`/emacs/init.el ~/.emacs.d/
ln -s `pwd`/emacs/Cask ~/.emacs.d/

curl https://raw.githubusercontent.com/emacs-jp/replace-colorthemes/master/dark-laptop-theme.el > `pwd`/emacs/themes/dark-laptop-theme.el

cd ~/.emacs.d
cask upgrade
cask install
cask update

cd ${START_PATH}

# install cpplint
curl https://raw.githubusercontent.com/google/styleguide/gh-pages/cpplint/cpplint.py > /usr/local/bin/cpplint.py
chmod 755 /usr/local/bin/cpplint.py

# install postgres
brew install postgresql
ln -sfv /usr/local/opt/postgresql/*.plist ~/Library/LaunchAgents

# install Google Test
curl -L https://github.com/google/googletest/archive/master.zip > /tmp/gtest.zip
cd /tmp
unzip gtest.zip
cd /tmp/googletest-master
cmake .
make
make install
cd ${START_PATH}

# install ESLint
npm install -g eslint eslint-config-google

# finishing
brew linkapps
brew cleanup
brew cask cleanup

echo Another application install manually
echo Android studio
echo Chrome
echo Dropbox
echo Firefox
echo SourceTree
echo VLC
