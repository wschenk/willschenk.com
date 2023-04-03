data "digitalocean_image" "docker-snapshot" {
  name = "packer-docker-0.0.1"
}

resource "digitalocean_droplet" "playserver" {
  name     = "playserver"
  image    = data.digitalocean_image.docker-snapshot.image
  size     = "s-1vcpu-1gb"
  region   = var.do_region
  ssh_keys = [
      var.ssh_fingerprint
  ]
}

output "ip" {
  value = "${digitalocean_droplet.playserver.ipv4_address}"
}