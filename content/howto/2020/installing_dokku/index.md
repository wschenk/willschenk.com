---
title: Installing dokku with Terraform and Ansible
subtitle: host your own heroku
tags:
  - howto
  - dokku
  - terraform
date: "2020-05-15"
draft: true
---

Dokku is a heroku like environment that you run on a single server that gives a lot of conviences that you get from Heroku. It's easy to deploy applications using `git push`, it manages the 12 factor app configuration, and lets you spin up and connect other servers like postgres and redis fairly easily.  Lets go through how to set this up.

## Install terraform

```bash
TF_VERSION=0.12.25
TF_ARCH=linux_amd64
cd /tmp
curl -o terraform.zip https://releases.hashicorp.com/terraform/${TF_VERSION}/terraform_${TF_VERSION}_${TF_ARCH}.zip
unzip terraform.zip
sudo mv terraform /usr/local/bin
```

Run `terraform version` to make sure that it all works.

## Install Ansible

There are a number of different wants to [install Ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html#installing-ansible-with-pip) but here's the way to do it with PIP:

```bash
curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
python get-pip.py --user
echo "export PATH=$HOME/.local/bin:$PATH" >> ~/.profile
export PATH=$HOME/.local/bin:$PATH
pip install --user ansible
```

Run `ansible --version` to make sure that you can run it.

## Setting up `env.tfvars` file

1. do token
2. dnsimple token

## `droplets.rf`

## `dnsimple.tf`

## Provision the infrastructure

```bash
terraform apply -var-file=env.tfvars
```

## Install dokku and plugins

```bash
bash setup.bash
```

## Setup the hostname

go to your dokku host with a webbrowser
select virtual domains
change the root domain
press save

## Create the application

On dokku host
dokku apps:create ruby
dokku postgres:create railsdatabase
dokku postgres:link railsdatabase ruby
dokku redis:create redisdb
dokku redis:link redisdb ruby

## Get application code

On localhost

cd /tmp
git clone https://github.com/heroku/ruby-getting-started
git remote add dokku dokku@dokku.willschenk.com:ruby
git push dokku master


## Add SSL

On dokku host

dokku config:set --global DOKKU_LETSENCRYPT_EMAIL=wschenk@gmail.com
dokku letsencrypt:cron-job --add
dokku letsencrypt ruby


## Install `terraform-inventory`

```bash
go get github.com/adammck/terraform-inventory
```

Test it out with

```bash
TF_STATE=. terraform-inventory --list | jq .
```

## Ping ansible

```bash
TF_STATE=. ansible -i `which terraform-inventory` -m ping -u root all
```

## Run the playbook

```bash
TF_STATE=. ansible-playbook -i `which terraform-inventory` -u root  playbooks/user_account.yml 
```



## Ansible galaxy

ansible-galaxy install dokku_bot.ansible_dokku

TF_STATE=. ansible-playbook -i `which terraform-inventory` -u root  playbooks/dokku.yml 


## References

1. https://developers.digitalocean.com/documentation/changelog/api-v2/new-size-slugs-for-droplet-plan-changes/
2. https://github.com/dokku/dokku-letsencrypt

