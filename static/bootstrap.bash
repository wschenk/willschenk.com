#!/bin/bash
#
# Written by Will Schenk <wschenk@gmail.com>
# https://willschenk.com

cat <<WELCOME
Welcome to the chromebook/debian bootstrapper.

This will create aliases for a number of common utilities that I use that will
install and configure the packages when run for the first time.

At the end of this, be sure to run
. ~/.profile
to make sure that your environment is up to date.

WELCOME

if ! command -v git > /dev/null; then
  echo Installing git
  sudo apt-get install git
fi

if test -z $(git config --get user.email); then
  read -p "Email Address (for git): " gitemail
  git config --global user.email $gitemail
fi

if test -z $(git config --get user.name); then
  read -p "Full name (for git)    : " gitname
  git config --global user.name $gitname
fi

function yesno() {
	read -p "$1 " YN

	[ "$YN" == "y" ]
}

if [ ! -f ~/.ssh/id_rsa ]; then
  if yesno "Set up ~/.ssh/id_rsa?"; then
    echo Paste in your id_rsa and them press ^D
    mkdir -p ~/.ssh
    cat > ~/.ssh/id_rsa
    echo Thank you.
    echo
  fi
fi

if [ ! -f ~/.ssh/id_rsa.pub ]; then
  if yesno "Set up ~/.ssh/id_rsa.pub?"; then
    echo Paste in your id_rsa.pub and them press ^D
    mkdir -p ~/.ssh
    cat > ~/.ssh/id_rsa.pub
    echo Thank you.
    echo
  fi
fi

# Set permissions
if [ -d ~/ssh ]; then
  chmod 700 ~/.ssh
  chmod 600 ~/.ssh/*
fi

echo
echo Creating ~/.bootstrap_functions

echo "if [ -f ~/.bootstrap_functions ]; then . ~/.bootstrap_functions; fi" >> ~/.profile

echo Final step, update your shells environmet by running
echo
echo source ~/.profile

cat > ~/.bootstrap_functions <<'END_ALIASES'
alias tmux='verify_tmux'
alias ping='verify_ping'
alias ipfs='verify_ipfs'
alias docker='verify_docker'
alias atom='verify_atom'
alias go='verify_go'
alias hugo='verify_hugo'
# alias nvm='verify_nvm'
# alias node='verify_node'
alias rbenv='verify_rbenv'
alias heroku='verify_heroku'
alias gcloud='verify_gcloud'

function verify_tmux() {
  cmd=$(which tmux)
  if [ -z "$cmd" ]; then sudo apt-get install tmux; fi
  $(which tmux) $@
}

function verify_ping() {
  cmd=$(which ping)
  if [ -z "$cmd" ]; then sudo apt-get install iputils-ping; fi
  $(which ping) $@
}

function verify_ipfs() {
  cmd=$(which ipfs)
  if [ -z "$cmd" ]; then install_ipfs; fi
  $(which ipfs) $@
}

function install_ipfs() {
  (
  echo
  echo "Installing ipfs"
  cd /tmp
  wget https://dist.ipfs.io/go-ipfs/v0.4.15/go-ipfs_v0.4.15_linux-amd64.tar.gz
  tar xzvf go-ipfs_v0.4.15_linux-amd64.tar.gz
  cd go-ipfs
  sudo ./install.sh
  )
}

function verify_docker() {
  cmd=$(which docker)
  if [ -z "$cmd" ]; then install_docker; fi
  if [ -z "$(groups | grep docker)" ]; then
    echo You need to log out and back in to make sure that you are in the docker groups
  else
    $(which docker) $@
  fi
}

function install_docker() {
  echo
  echo "Installing Docker"
  # Update debian
  sudo apt-get update
  sudo apt-get upgrade -y

  sudo apt-get install -y \
       apt-transport-https \
       ca-certificates \
       curl \
       gnupg2 \
       software-properties-common

  # Add the gpg key if it;s not there
  curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -

  # Add the docker repository
  sudo add-apt-repository \
     "deb [arch=amd64] https://download.docker.com/linux/debian \
     $(lsb_release -cs) \
     stable"

  # Update and add docker-ce
  sudo apt-get update
  sudo apt-get install -y docker-ce

  # Install docker-compose
  sudo curl -L "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$(uname -s)-$(uname -m)" \
    -o /usr/local/bin/docker-compose

  # Add yourself to the docker group if you aren't already
  sudo groupadd docker
  sudo usermod -aG docker $USER
}

function verify_atom() {
  cmd=$(which atom)
  if [ -z "$cmd" ]; then install_atom; fi
  $(which atom) $@
}

function install_atom() {
  (
  echo
  echo Installing atom
  cd /tmp
  sudo apt-get install wget
  wget https://atom.io/download/deb
  mv deb atom.deb
  sudo apt install -y ./atom.deb
  )
}

function verify_nvm() {
  cmd=$(which node)
  if [ -z "$cmd" ]; then install_nvm; fi
  unalias nvm
  nvm $@
}

function install_nvm() {
  (
  echo
  echo Installing NVM
  curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | bash
  )
  source ~/.profile
  nvm install 10
  nvm global 10
}

function verify_rbenv() {
  cmd=$(which rbenv)
  if [ -z "$cmd" ]; then install_rbenv; fi
  $(which rbenv) $@
}

function install_rbenv() {
  echo
  echo Installing rbenv

  sudo apt-get install -y libssl-dev libreadline-dev zlib1g-dev

  curl -fsSL https://github.com/rbenv/rbenv-installer/raw/master/bin/rbenv-installer | bash
  echo 'export PATH=$PATH:$HOME/.rbenv/bin' >> $HOME/.profile
  echo 'eval "$(rbenv init -)"' >> $HOME/.profile
  source ~/.profile
  rbenv install 2.5.5
  rbenv global 2.5.5
}


function verify_go {
  cmd=$(which go)
  if [ -z "$cmd" ]; then install_go; fi
  $(which go) $@
}

function install_go() {
  (
  echo
  echo Installing go
  cd /tmp
  wget https://dl.google.com/go/go1.12.1.linux-amd64.tar.gz
  sudo tar -C /usr/local -xzf go1.12.1.linux-amd64.tar.gz
  echo 'export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin' >> $HOME/.profile
  )
  export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin
  source ~/.profile
}

function verify_hugo {
  cmd=$(which hugo)
  if [ -z "$cmd" ]; then install_hugo; fi
  $(which hugo) $@
}

function install_hugo() {
  (
  echo
  echo Installing hugo
  cd /tmp
  rm -rf hugo
  git clone https://github.com/gohugoio/hugo.git
  cd hugo
  go install
  )
}

function verify_heroku() {
  cmd=$(which heroku)
  if [ -z "$cmd" ]; then install_heroku; fi
  $(which heroku) $@
}

function install_heroku() {
  echo
  echo Installing heroku
  curl https://cli-assets.heroku.com/install-ubuntu.sh | sh
}

function verify_gcloud() {
  cmd=$(which gcloud)
  if [ -z "$cmd" ]; then install_gcloud; fi
  $(which gcloud) $@
}

function install_gcloud() {
  # Create environment variable for correct distribution
  export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)"

  # Add the Cloud SDK distribution URI as a package source
  echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

  # Import the Google Cloud Platform public key
  curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -

  # Update the package list and install the Cloud SDK
  sudo apt-get update && sudo apt-get install -y google-cloud-sdk
}
END_ALIASES
