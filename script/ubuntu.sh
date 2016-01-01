
START_PATH=${PWD}

sudo apt-get -y update
sudo apt-get -y upgrade

# install misc
sudo apt-get -y install libboost1.55-dev libboost-system1.55-dev libboost-date-time1.55-dev libboost-random1.55-dev clang-3.6 cmake curl doxygen dropbox llvm-3.6 llvm-3.6-dev llvm-3.6-runtime llvm-3.6-tools lv libssl-dev libffi-dev libuv-dev libedit-dev

# install Ricty font
if [ ! -e ~/.fonts/Ricty* ]; then
sudo apt-get -y install fontforge fonts-inconsolata fonts-migmix
curl -L https://github.com/yascentur/Ricty/archive/master.zip > /tmp/ricty.zip
unzip -d /tmp/ /tmp/ricty.zip
cd /tmp/Ricty-master
./ricty_generator.sh auto
mkdir ~/.fonts
cp -f Ricty*.ttf ~/.fonts/
fc-cache -vf

cd ${START_PATH}
fi

# install emacs
sudo apt-get -y install emacs
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python 

mkdir ~/.emacs.d

ln -s `pwd`/emacs/init.el ~/.emacs.d/
ln -s `pwd`/emacs/Cask ~/.emacs.d/

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

# install electron
sudo apt-get -y install nodejs npm
sudo update-alternatives --install /usr/bin/node node /usr/bin/nodejs 10
sudo npm install electron-prebuilt -g

# finishing
sudo apt-get autoclean
