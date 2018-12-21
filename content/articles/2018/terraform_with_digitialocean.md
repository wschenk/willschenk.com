---
title: "Terraform and Packer with Digitalocean"
date: 11-23-2018
draft: true
tags:
  - howto
  - terraform
  - packer
  - devops
---

Terraform orchastrates the setting up of your infrastructure, and packer helps you build images.  We are going to use packer to create an image, and then deploy it using terrform.

Install terraform from [the download page](https://www.terraform.io/downloads.html).  I will put it in `/usr/local/bin`.

```bash
$ cd /tmp
$ wget https://releases.hashicorp.com/terraform/0.11.10/terraform_0.11.10_linux_amd64.zip
$ unzip terraform_0.11.10_linux_amd64.zip
$ sudo mv terraform /usr/local/bin
```

We need to get an access token for digitalocean.  Log into your account at cloud.digitalocean.com and go to `API` on the left.  Generate a new access token.  Copy this token and put it in the environment:


```bash
export TF_VAR_do_token=9a4d341ec0a512bfef2ae9....
export TF_VAR_pub_key=$HOME/.ssh/id_rsa.pub
export TF_VAR_pvt_key=$HOME/.ssh/id_rsa
export TF_VAR_ssh_fingerprint=$(ssh-keygen -E md5 -lf $TF_VAR_pub_key | awk '{print $2}' | sed 's/MD5://')
export DO_REGION='nyc3'
```

Now lets create a working directory.

```bash
$ mkdir blog
$ cd blog
```

`provider.tf`

```tf
variable "do_token" {}
variable "pub_key" {}
variable "pvt_key" {}
variable "ssh_fingerprint" {}

provider "digitalocean" {
  token = "${var.do_token}"
}
```

`www.tf`

```tf
resource "digitalocean_droplet" "www" {
    image = "ubuntu-18-10-x64"
    name = "www"
    count = 2
    region = "nyc3"
    size = "s-1vcpu-1gb"
    private_networking = true
    ssh_keys = [
      "${var.ssh_fingerprint}"
    ]

    connection {
      user = "root"
      type = "ssh"
      private_key = "${file(var.pvt_key)}"
      timeout = "2m"
  }

    provisioner "remote-exec" {
    inline = [
      "export PATH=$PATH:/usr/bin",
      # install nginx
      "sudo apt-get update",
      "sudo apt-get -y install nginx"
    ]
  }
}
```




## Packer

```bash
$ cd /tmp
$ wget https://releases.hashicorp.com/packer/1.3.2/packer_1.3.2_linux_amd64.zip
$ unzip packer_1.3.2_linux_amd64.zip
$ sudo mv packer /usr/local/bin
```

```json
{
  "variables": {
    "api_token": "{{env `TF_VAR_do_token`}}",
    "region": "{{ env `DO_REGION`}}"
  },
  "builders": [{
    "type": "digitalocean",
    "api_token": "{{user `api_token`}}",
    "image": "ubuntu-18-10-x64",
    "region": "{{ user `region` }}",
    "size": "512mb",
    "ssh_username": "root"
  }]
}
```


---

References

1. https://www.digitalocean.com/community/tutorials/how-to-use-terraform-with-digitalocean
