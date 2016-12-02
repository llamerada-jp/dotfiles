
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
START_PATH=${PWD}

brew update
brew upgrade

brew install pkg-config
brew install boost
brew install automake
brew install cask
brew install cmake
brew install doxygen
brew install libuv
brew install llvm
brew install lv
brew install node
brew install nginx
brew install socat
brew install valgrind
brew cask install xquartz

# set git user
git config --global user.name "Yuji Ito"
git config --global user.email llamerada.jp@gmail.com

# install Ricty font
brew tap sanemat/font
brew install ricty

cp -f /usr/local/Cellar/ricty/*/share/fonts/Ricty*.ttf ~/Library/Fonts/
fc-cache -vf

# install emacs
brew install --cocoa --srgb --with-gnutls --japanese emacs
brew linkapps emacs

mkdir ~/.emacs.d
mkdir ~/Develop

ln -s `pwd`/emacs/init.el ~/.emacs.d/
ln -s `pwd`/emacs/Cask ~/.emacs.d/

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
