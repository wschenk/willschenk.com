variable "linode_token" {}

provider "linode" {
  token = var.linode_token
}

resource "linode_sshkey" "key" {
  label = "sshkey"
  ssh_key = chomp(file("~/.ssh/id_rsa.pub"))
}

resource "linode_instance" "terraform-web" {
  image = "linode/ubuntu18.04"
  label = "Terraform-Web-Example"
  group = "Terraform"
  region = "us-east"
  type = "g6-standard-1"
  authorized_keys    = ["${linode_sshkey.key.ssh_key}"]
#  root_pass = "YOUR_ROOT_PASSWORD"
}

output "server_ip" {
  value = "${linode_instance.terraform-web.ip_address}"
}