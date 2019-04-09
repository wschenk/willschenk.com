---
title: Setting up a chromebook for development
subtitle: Documenting my steps
date: "2019-03-30"
tags:
  - howto
  - chromebook
  - docker
---

I just switch my pixelbook back to the stable channel, and this is what I did to get back to developing on it.

<!--more-->

## First:

1. Open up settings
2. Linux (Beta)
3. Turn on
4. Wait for it to download
5. Startup the terminal app

Everything from here happens in the terminal app.

## Configure git

```
git config --global user.email "you@example.com"
git config --global user.name "Your Name"
```

## Install Docker

Install docker the normal way

```bash
# Update debian
sudo apt-get update
sudo apt-get upgrade

sudo apt-get install \
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
sudo apt-get install docker-ce

# Install docker-compose
sudo curl -L "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$(uname -s)-$(uname -m)" \
  -o /usr/local/bin/docker-compose

# Add yourself to the docker group if you aren't already
sudo groupadd docker
sudo usermod -aG docker $USER
```

Now you may need to shutdown the linux installation to make sure that your user is in the docker group.

1. Alt-click on the terminal icon in the dock
2. Select "Shutdown Linux (Beta)"
3. Restart terminal
4. Run `docker run hello-world` to test out the installation

## Installing atom

```
cd /tmp
sudo apt-get install wget
wget https://atom.io/download/deb
mv deb atom.deb
sudo apt install ./atom.deb
```

Test out the installation using `atom`.

## Installing SublimeText

```
sudo apt-get install wget
wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | sudo apt-key add -
echo "deb https://download.sublimetext.com/ apt/stable/" | sudo tee /etc/apt/sources.list.d/sublime-text.list
sudo apt-get update
sudo apt-get install sublime-text
```

Test out the installation using `subl`

## Installing nvm and node

```
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | bash
source ~/.profile
nvm install 10
```

Test out using `node -v`

## Installing rbenv and ruby

```
sudo apt-get install -y libssl-dev libreadline-dev zlib1g-dev

curl -fsSL https://github.com/rbenv/rbenv-installer/raw/master/bin/rbenv-installer | bash
echo 'export PATH=$PATH:$HOME/.rbenv/bin' >> $HOME/.profile
echo 'eval "$(rbenv init -)"' >> $HOME/.profile
source ~/.profile
rbenv install 2.5.5
rbenv global 2.5.5
```

Test out using `ruby --version`


## Installing go

```
cd /tmp
wget https://dl.google.com/go/go1.12.1.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.12.1.linux-amd64.tar.gz
echo 'export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin' >> $HOME/.profile
source ~/.profile
```

Test out using `go version`

## Installing hugo

```
cd /tmp
git clone https://github.com/gohugoio/hugo.git
cd hugo
go install
```

## Installing Heroku Toolbelt

```
curl https://cli-assets.heroku.com/install-ubuntu.sh | sh
```

Test out using `heroku login`

## Installing Google Cloud CLI

```
# Create environment variable for correct distribution
export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)"

# Add the Cloud SDK distribution URI as a package source
echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

# Import the Google Cloud Platform public key
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -

# Update the package list and install the Cloud SDK
sudo apt-get update && sudo apt-get install google-cloud-sdk
```

Test out using `gcloud auth login`

## Installing Android Studio

1. Visit https://developer.android.com/studio/
2. Accept the terms in order to download
3. Open up the Files app
4. Drag into linux files
5. In the terminal, unzip the file. (In my case `unzip android-studio-ide-182.5314842-linux.zip`)
6. `cd android-studio/bin`
7. `./studio.sh`

## Installing ipfs

```
wget https://dist.ipfs.io/go-ipfs/v0.4.15/go-ipfs_v0.4.15_linux-amd64.tar.gz
tar xzvf go-ipfs_v0.4.15_linux-amd64.tar.gz
cd go-ipfs
sudo ./install.sh
```

Test out using `ipfs`

## Actually it's all standard stuff

There's nothing specifically Chromebook about this, but its easier for me to keep this all written down in one place.
