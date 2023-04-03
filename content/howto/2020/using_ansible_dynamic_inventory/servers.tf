variable "dns_token" {}
variable "dns_account_id" {}
variable "dns_domain" {}
variable "linode_token" {}

provider "dnsimple" {
  token = var.dns_token
  account = var.dns_account_id
}

provider "linode" {
  token = var.linode_token
}

resource "dnsimple_record" "web1" {
  domain = var.dns_domain
  name   = "web1"
  value  = linode_instance.web1.ip_address
  type   = "A"
  ttl    = 3600
}

resource "dnsimple_record" "web2" {
  domain = var.dns_domain
  name   = "web2"
  value  = linode_instance.web2.ip_address
  type   = "A"
  ttl    = 3600
}

resource "dnsimple_record" "db1" {
  domain = var.dns_domain
  name   = "db1"
  value  = linode_instance.db1.ip_address
  type   = "A"
  ttl    = 3600
}

# Setup the ssh key from the local machine
resource "linode_sshkey" "key" {
  label = "sshkey"
  ssh_key = chomp(file("~/.ssh/id_rsa.pub"))
}

# Create a server
resource "linode_instance" "web1" {
  image = "linode/debian10"
  label = "web1"
  group = "web"
  tags = ["web"]
  region = "us-east"
  type = "g6-standard-1"
  authorized_keys    = ["${linode_sshkey.key.ssh_key}"]
}

resource "linode_instance" "web2" {
  image = "linode/debian10"
  label = "web2"
  group = "web"
  tags = ["web"]
  region = "us-east"
  type = "g6-standard-1"
  authorized_keys    = ["${linode_sshkey.key.ssh_key}"]
}

resource "linode_instance" "db1" {
  image = "linode/debian10"
  label = "db1"
  group = "db"
  tags = ["database"]
  region = "us-east"
  type = "g6-standard-1"
  authorized_keys    = ["${linode_sshkey.key.ssh_key}"]
}
