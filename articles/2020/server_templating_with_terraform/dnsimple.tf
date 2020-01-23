variable "dns_token" {}
variable "dns_account_id" {}
variable "dns_domain" {}

provider "dnsimple" {
  token = var.dns_token
  account = var.dns_account_id
}

resource "dnsimple_record" "web" {
  domain = var.dns_domain
  name   = "web"
  value  = linode_instance.web.ip_address
  type   = "A"
  ttl    = 3600
}