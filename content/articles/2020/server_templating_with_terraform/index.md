---
title: Template to setup a linode server with DNS and HTTPS
subtitle: use terraform to coordinate stuff
tags:
  - howto
  - scripting
  - terraform
  - linode
  - dns
date: "2020-01-23"
remote: git@github.com:wschenk/terraform_ssl_server_template.git
repository: https://github.com/wschenk/terraform_ssl_server_template
---

I use [DNSimple](https://dnsimple.com/) for domain management, and I've been playing around with a bunch of different cloud providers and deployment setups. So I wanted to make it easier to give these machines names rather than clicking through the control panel all the time.  Lets walk through how to use terraform, DNSimple, and linode to provision and new machine and give it a name on the internet, and then create a webserver on it to which encrypts traffic.

This is really a base template for easily spinning up simple sites.

1. Get a domain and host it on DNSimple
2. Sign up for ISP, in this case Linode
3. Get API token for linode
4. Terraform up your server and get it's ip address
5. Get api tokens from DNSimple
6. Terraform up DNSimple to point to your ip address
7. Setup nginx with letsencrypt to have secure hosting


## Setting up linode

Create a new linode account if you don't have one.  Then go to your [Linode Profile Token Page](https://cloud.linode.com/profile/tokens) and create a personal access token. Again copy it some where secure and then set it in the current environment. After this everything will be scripted and automated.

```bash
export LINODE_TOKEN=asdfasdf
```

In order to pass variables into terraform, you need to prefix them with `TF_VAR_`. Lets do that now.

```bash
export TF_VAR_linode_token=$LINODE_TOKEN
```

In this script, we are setting up a Debian linode server with our local ssh keys installed. (It installs the public key found in `~/.ssh/id_rsa.pub`.)

[`linode.tf`](`linode.tf`)
{{% code file="articles/2020/server_templating_with_terraform/linode.tf" language="tf" %}}

Running `terraform init` will validate this file and make sure that the right plugins are installed. You can then set up the server but running `terraform apply`.

Once it's up, you can test connecting to it using

```bash
ssh root@$(terraform output server_ip)
```

## Get a DNSimple token

I already use DNSimple to host my domains, so that's what I'm going to use here. DNSimple is cool but many places offer domains and I have no special insight as to why one is better than the other.  Hosting the domain at your cloud provider is probably preferable if you are committed to one, but I host things all over the place.

Log in to your [DNSimple Account Page](https://dnsimple.com/) and create a new account access token. Go to the `Account` tab, then on the left select `Automation`


```bash
export DNS_TOKEN=asdfasdfasdf
```

Now we can test out the token and look for our account id, which is displayed on the page but why not just verify that things are looking good.

```bash
curl https://api.dnsimple.com/v2/whoami -H "Authorization: Bearer ${DNS_TOKEN}" | jq .
```

Which should make something like:

```json
{
  "data": {
    "user": null,
    "account": {
      "id": 7008,
      "email": "wschenk@gmail.com",
      "plan_identifier": "gold-v1-yearly",
      "created_at": "2012-06-07T11:47:25Z",
      "updated_at": "2020-01-04T18:11:05Z"
    }
  }
}
```

You can set and environment variable automatically with

```bash
export DNS_ACCOUNT_ID=$(curl https://api.dnsimple.com/v2/whoami -H "Authorization: Bearer ${DNS_TOKEN}"|jq .data.account.id)
```

To get a list of domains

```bash
curl https://api.dnsimple.com/v2/${DNS_ACCOUNT_ID}/domains -H "Authorization: Bearer ${DNS_TOKEN}"|jq .
```

Now that we have a working token and account id, we can use terraform to setup a name that points to our new server.

```bash
export TF_VAR_dns_token=$DNS_TOKEN
export TF_VAR_dns_account_id=$DNS_ACCOUNT_ID 
export TF_VAR_dns_domain=willschenk.com
```

[`dnsimple.tf`](`dnsimple.tf`)
{{% code file="articles/2020/server_templating_with_terraform/dnsimple.tf" language="tf" %}}

Then run `terraform init` to download the DNSimple provisioner, and `terraform apply` to set the `web` address of the `TF_VAR_dns_domain` domain to the public IP that linode gave you.

Terraform is also smart enough to order the dependacies, so if you setup everything from scratch it will setup the server first in order to get the IP address that it needs to setup the domain record. Nifty.

## Setting up the server

Next we are going to run a script over SSH to do the provisioning of the `Debian` instance.  I think that this is easier that using packer or some other tool, since we only have a few commands that need to run.  We should setup the script so that it can run multiple times without any ill effect.  The trick here is that if we really change things, we should backup the data on the server and completely redeploy everything from scratch.  You don't want to manually login to the server to make changes really at any point.

This is what we're going to do:

1. Set the Fully Qualified Domain Name
2. Update all system packages (this can take a while the first run)
3. Install `nginx`
4. Install `certbot` from it's repository
5. Run the `certbot` command, which will prompt you to fill out information.


[`setup.bash`](`setup.bash`)
{{% code file="articles/2020/server_templating_with_terraform/setup.bash" language="bash" %}}

Run this with `bash setup.bash` and it will copy itself over to the remote server and run the setup scripts. It may take a minute or two for the remote server to be up and accepting ssh connections. 

I set the server to redirect everything to HTTPS. With this baseline you can dump in your static files, add subdomains, or do whatever you want.

## Now you have a server

You need a domain name to get HTTPS, and there are a lot of services that require that.  This is a simple template to get you up and running.  From here you can expirement and shut it down, or you can use this as a base to build something else up. (This post started because I wanted to play around with [dokku](http://dokku.viewdocs.io/dokku/) and I got distracted setting up a server and domain.  Now I can spin something up quicky and play, and if I don't want to keep anything I can do `terraform destroy` and it all goes away.

I find that it's actually nice to use scripts to set things up and tear things down. A lot of these blog posts are actually me going through the steps of setting things up over and over, and recreating the process a number of times to make sure that I understand how it works.


## References

1. https://www.linode.com/docs/applications/configuration-management/how-to-build-your-infrastructure-using-terraform-and-linode/
2. https://www.terraform.io/docs/providers/linode/r/sshkey.html
2. https://www.terraform.io/docs/providers/dnsimple/r/record.html
4. https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-18-04
5. https://certbot.eff.org/lets-encrypt/ubuntubionic-nginx
