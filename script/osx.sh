
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
brew install caskroom/cask/brew-cask

brew cask install android-studio
brew cask install dropbox
brew cask install firefox
brew cask install google-chrome
brew cask install google-photos-backup
brew cask install google-japanese-ime
brew cask install eclipse-java
brew cask install magican
brew cask install opera
brew cask install sourcetree
brew cask install virtualbox
brew cask install vlc
brew cask install xquartz

# install Ricty font
brew tap sanemat/font
brew install ricty

cp -f /usr/local/Cellar/ricty/*/share/fonts/Ricty*.ttf ~/Library/Fonts/
fc-cache -vf

# install emacs
brew install --cocoa --srgb --with-gnutls --japanese emacs

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

# finishing
brew linkapps
brew cleanup
