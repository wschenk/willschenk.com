---
title: Using Ansible's Dynamic Inventory
subtitle: plugging in terraform and ansible together
tags:
  - howto
  - terraform
  - ansible
  - linode
date: "2020-01-24"
draft: true
---

In the last post we used terraform and a simple bash script to provision a webserver, set it's hostname, and get a webserver running with letsencrypt. The script is fine, but you have to remember to run it everytime and its not totally idempotent so if you run it again to make a change we don't really know if the server is in a good state.

Ansible is a tool that makes this possible, and is really especially useful when you have lots of machines that you are managing. For our case this is overkill but lets see how we can get Ansible working with an environment that is managed by terraform.

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

## Create your environment

For this you need the Linode API key as well as DNSimple API key as described in the last post.  In this example, we are creating 3 servers, web1, web2, and db1.  The web servers are tagged with "web" and the "db" is tagged with "db", which is how we'll group them when we use ansible to talk with them -- by pulling the tags from the Linode API.

[`servers.tf`](`servers.tf`)
{{% code file="articles/2020/using_ansible_dynamic_inventory/servers.tf" language="tf" %}}

Be sure to set the `TF_VAR_` environment variables

- `TF_VAR_dns_token` - DNSimple API key
- `TF_VAR_dns_account_id` - Account ID
- `TF_VAR_dns_domain` - The domain that these servers are listed under
- `TF_VAR_linode_token` - Linode API Key

Then do `terraform init`, `terraform plan` to make sure you really want to do this, and `terraform apply` to make it happen.

## Ansible dynamic inventory script

We're going to use the [terraform-inventory](https://github.com/adammck/terraform-inventory) package to read the `terraform.tfstate` file.

You'll need [golang installed on your computer](https://golang.org/doc/install#install) to install this using:

```bash
go get github.com/adammck/terraform-inventory
```

On OSX you should also be able to `brew install terraform-inventory`.

We can test this out by running the command and passing the output to JQ for some nice formatting. Note we are setting the `TF_STATE` variable to the current directory, and this will trigger `terraform-inventory` to read the output of the `terraform` command itself rather than reading the statefile directly. So if you have set up terraform to use a remote store this will work also.

```bash
TF_STATE=. terraform-inventory --list | jq .
```

With this we should be able to test out running `ansible` against different servers.  First lets run a `ping` command on all of them to make sure that we have connectivity. Since this is the first time we've connected to these machines we'll have to deal with the SSH knownhost file good times.

```bash
TF_STATE=. ansible -i `which terraform-inventory` -m ping -u root all
```

And here you'll need to type `yes` 3 times to setup the `known_hosts` file for these new machines.  You should get responses back in green! 

If we want to limit the command to run only on specific servers, we can specify a tag. On my computer for some reason these are prefixed with `0_` so it looks like this:

```bash
TF_STATE=. ansible -i `which terraform-inventory` -m ping -u root 0_web
```

```bash
TF_STATE=. ansible -i `which terraform-inventory` -m ping -u root 0_database
```

## Applying playbooks



## References

1. https://alex.dzyoba.com/blog/terraform-ansible/
2. https://github.com/adammck/terraform-inventory
3. https://gist.github.com/mattiaslundberg/ba214a35060d3c8603e9b1ec8627d349
4. https://linuxhint.com/ansible-tutorial-beginners/
5. https://www.linuxtechi.com/manage-ansible-static-and-dynamic-host-inventory/
