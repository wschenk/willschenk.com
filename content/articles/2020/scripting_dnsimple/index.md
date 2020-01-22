---
title: Automating DNS, linode, and letsencrypt
subtitle: easily spin up a server
tags:
  - howto
  - scripting
  - terraform
  - dns
date: "2020-01-22"
draft: true
---

I use DNSSimple for domain management, and I've been playing around with a bunch of different cloud providers and deployment setups. So I wanted to make it easier to give these machines names rather than clicking through the control panel all the time.  Lets walk through how to use terraform, dnsimple, and linode to provision and new machine and give it a name on the internet, and then create a webserver on it to which encrypts traffic.

This is really a base template for easily spinning up simple sites.

## Get a dnsimple token

Log in to your [DNSimple Account Page](https://dnsimple.com/) and create a new account access token. Go to the `Account` tab, then on the left select `Automation`


```bash
export DNS_TOKEN=asdfasdfasdf
```

Now we can test out the token and look for our account id, which is displayed on the page but why not just verify that things are looking good.

```bash
curl -i https://api.dnsimple.com/v2/whoami -H "Authorization: Bearer ${DNS_TOKEN}" | jq .
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

## Setting up linode

First we need to go to your [Linode Profile Token Page](https://cloud.linode.com/profile/tokens) and create a personal access token. Again copy it some where secure and then set it in the current environment.

```bash
export LINODE_TOKEN=asdfasdf
```



## References

1. https://www.linode.com/docs/applications/configuration-management/how-to-build-your-infrastructure-using-terraform-and-linode/
2. https://www.terraform.io/docs/providers/linode/r/sshkey.html
2. https://www.terraform.io/docs/providers/dnsimple/r/record.html
