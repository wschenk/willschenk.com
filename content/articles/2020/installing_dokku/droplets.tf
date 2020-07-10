variable "do_token" {
  description = "digitalocean access token"
  type = string
}

variable "do_region" {
  description = "region to create resources"
  type = string
}

variable "ssh_fingerprint" {
  description = "which ssh key to intall on the server"
  type = string
}

provider "digitalocean" {
  token             = var.do_token
}

resource "digitalocean_droplet" "dokku" {
  name     = "dokku"
  image    = "debian-10-x64"
  size     = "s-2vcpu-2gb"
#  monitoring = true
  region   = var.do_region
  ssh_keys = [
      var.ssh_fingerprint
  ]
}

resource "digitalocean_droplet" "pub" {
  name     = "pub"
  image    = "debian-10-x64"
  size     = "s-2vcpu-4gb"
#  monitoring = true
  region   = var.do_region
  ssh_keys = [
      var.ssh_fingerprint
  ]
}

output "ip" {
  value = "${digitalocean_droplet.dokku.ipv4_address}"
}
