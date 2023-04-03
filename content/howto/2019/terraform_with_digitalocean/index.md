---
title: "Terraform and Packer with Digital Ocean"
subtitle: "Automate all the things"
date: "2019-12-24"
tags:
  - terraform
  - packer
  - devops
aliases:
  - "/articles/2019/terraform_with_digitalocean"
---

Terraform orchastrates the setting up of your infrastructure, and packer helps you build images. We are going to setup terraform to work with Digital Ocean, and then use package to create and image and deploy it.

## Install terraform

Install terraform from [the download page](https://www.terraform.io/downloads.html). I'm on Linux, so we'll download the latest version and put it in `/usr/local/bin`.

```bash
cd /tmp
wget https://releases.hashicorp.com/terraform/0.12.18/terraform_0.12.18_linux_amd64.zip
unzip terraform_0.12.18_linux_amd64.zip
sudo mv terraform /usr/local/bin
```

## Setup Digital Ocean API

We need two tokens. One for accessing the Digital Ocean API for infrastructure, and the other to access the spaces API.

Log into your account at [cloud.digitalocean.com](cloud.digitalocean.com) and go to `API` on the left. Generate a new access token with read and write privileges.

Copy this token and keep it somewhere safe. Our terraform scripts will access this token, but we'll keep the actual values in the environment rather than hardcoding them. We'll be able to pass things in when prefixed with `TF_VAR`.

Then create a Spaces API key, which will have an Access Token and a Secret Key.

We are going to setup 4 environment variables that we will use for terraform.

1. do_token: the actual API token
2. do_access_key: the spaces api access key
3. do_secret_key: the spaces api secret key
4. ssh_fingerprint: a finger print for installing a key on a droplet

Note that my public key is in `~/.ssh/id_rsa.pub`

```bash
export TF_VAR_do_token=9a4d341ec0a512bfef2ae9....
export TF_VAR_do_access_key=asdfasdf...
export TF_VAR_do_secret_key=asdfasd...
export TF_VAR_ssh_fingerprint=$(ssh-keygen -E md5 -lf ~/.ssh/id_rsa.pub | awk '{print $2}' | sed 's/MD5://')
```

## Create `provider.tf`

The first file we are going to use is [`provider.tf`](provider.tf):

{{< highlight "tf" >}}
{{% raw "provider.tf" %}}
{{< /highlight >}}

We are declaring the variables at top, which will look first in the environment for them and if not found will prompt you to enter a value. We are going to use the name for the space that we're storing the terraform state in multiple places, so we'll define it as a variable, though unforunately we'll need to hard code it when we setup the terraform backend.

Now run `terraform init`. This should download the `digitalocean` plugin in the `.terraform` directory, and we should be ready to start making terraform commands! When we do `terraform plan` it should say that everything is up to date.

## Create a secure store for holding the config

Now lets use terraform to create a digitalocean store to save our deployment state. [`spaces.tf`](spaces.tf):

{{< highlight "tf" >}}
{{% raw "spaces.tf" %}}
{{< /highlight >}}

As a reminder, space names need to be globally unique so you'll need to update your name to make it your own. Now run `terraform plan` to see what its going to do. You should see that it wants to create the space.

Run `terraform apply` to actually bring your environment up!

Once your space is up and running, lets configure terraform to use this as a backend. [`backend.tf`](backend.tf):

{{< highlight "tf" >}}
{{% raw "backend.tf" %}}
{{< /highlight >}}

We are using the S3 backend but pointing it to Digital Ocean's endpoint. If you don't know what endpoint you are using, you can see inside of the Digital Ocean console.

I don't want to keep my access keys inside this file, but we can't pull them out using variables due to the way that terraform works. So since we are pretending to be AWS, let's set the environment variables for AWS access to the keys that we got from the Spaces API.

_Note: While I don't use AWS and therefor don't mind messing with these keys, if you do use AWS from the shell you are working in this will point to the wrong place._

```bash
export AWS_ACCESS_KEY_ID=$TF_VAR_do_access_key
export AWS_SECRET_ACCESS_KEY=$TF_VAR_do_secret_key
```

We need to run `terraform init` again to pull down the right plugins and configure everything. If all goes well, you'll be asked to migrate your local state over. Go for it!

## Create a droplet

Now that we have the plumbing working, lets create an actual droplet to play with.

In [`droplets.tf.orig`](droplets.tf.orig):

{{< highlight "tf" >}}
{{% raw "droplets.tf.orig" %}}
{{< /highlight >}}

When going through this, rename to `droplet.rf`. Later, we will be editing this file and so this is the base version of it.

You can find a list of [Digital Ocean image and sizes](https://slugs.do-api.dev/) to choose from, here we are using `debian-10` and one of the smaller vms.

We also define an `output` named `ip` that pulls from the digital ocean API based on what has been created.

Additionally we have added an SSH key to the image.

So to connect, we can do

```bash
ssh "root@$(terraform output ip)"
```

And you should be able to connect to your server.

## Creating a custom image

From this point you should be able to add other services with terraform to wire up your application. We'll skip a more elaborate network and service configuration -- which is really where terraform makes sense -- since there are plenty of other places that document that.

Instead, lets look at how to build a custom image that, perhaps, would contain your application. We're going to use `packer` to build this image, on which we'll just install `docker`. There's already an official docker image in Digital Ocean so this is really just an excersize so you get the idea.

```bash
wget https://releases.hashicorp.com/packer/1.5.1/packer_1.5.1_linux_amd64.zip
unzip packer_1.5.1_linux_amd64.zip
sudo mv packer /usr/local/bin
```

Now we create a [`docker.json`](docker.json) file that we'll give to `packer` that will provision the instance for us. Inside you'll notice that I named the snapshot `packer-docker-0.0.1`. In a real build process, this should probably be passed into packer.

{{< highlight "json" >}}
{{% raw "docker.json" %}}
{{< /highlight >}}

And then a script that will be run on the build image that actually installs docker. [`docker_install.sh`](docker_install.sh):

{{< highlight "bash" >}}
{{% raw "docker_install.sh" %}}
{{< /highlight >}}

We also need to set the region as the same you have it in the `providers.tf` file.

```bash
export TF_VAR_do_region=nyc3
```

Then run `packer validate docker.json` to make sure that everything is copacitic. Once that's good, then run `packer build docker.json`.

## Telling terraform to use your image

Once this is done, use this new [`droplets.tf`](droplets.tf) file to deploy this image onto your droplet. The first stanza looks up the image from the snapshot name, and then we use that inside of the `image` attribute to tell `terraform` which droplet should be running.

{{< highlight "tf" >}}
{{% raw "droplets.tf" %}}
{{< /highlight >}}

Now lets test it out:

```bash
terraform apply
```

And then connect:

```bash
ssh "root@$(terraform output ip)"
```

Once it connects, you should be able to run on the host machine:

```bash
docker run --rm hello-world
```

At which point we end our tutorial!

## Cleanup

To clean up everything, you can run `terraform destroy`. This will leave the snapshot, which was created by `packer` and not managed by `terraform`, and it will probably leave the space that we created since its not empty. Which is fine, since its better to keep the state around anyway, these things can be cleaned up by hand.

This is basically just a howto to get things initially setup, there's a lot more you can do with organizing your files and managing different environments. The droplet that we created is on the public internet which is where most of my toys live, and we didn't explore the more interesting parts of terraform where you setup networks and other services. But most of the tutorials I've seen are for setting things up with AWS, and I wanted to see how to get it done with Digital Ocean.

---

References

1. https://www.digitalocean.com/community/tutorials/how-to-use-terraform-with-digitalocean
2. https://medium.com/@chris_linguine/dont-push-your-terraform-state-files-945cfdf71f88
3. https://www.terraformupandrunning.com/
4. https://dev.to/jmarhee/digitalocean-spaces-as-a-terraform-backend-3lck
5. https://slugs.do-api.dev/
6. https://github.com/gokhansengun/packer-terraform-demo
