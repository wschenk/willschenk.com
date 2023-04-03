variable "linode_token" {}

provider "linode" {
  token = var.linode_token
}

# Setup the ssh key from the local machine
resource "linode_sshkey" "key" {
  label = "sshkey"
  ssh_key = chomp(file("~/.ssh/id_rsa.pub"))
}

# Create a server
resource "linode_instance" "web" {
  image = "linode/debian10"
  label = "Web"
  group = "Terraform"
  region = "us-east"
  type = "g6-standard-1"
  authorized_keys    = ["${linode_sshkey.key.ssh_key}"]
  # Leave the root password unset to keep it random
#  root_pass = "YOUR_ROOT_PASSWORD"
}

output "server_ip" {
  value = "${linode_instance.web.ip_address}"
}

