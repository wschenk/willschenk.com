variable "dnsimple_token" {
  description = "dnssimple api access token"
}

variable "dnsimple_account_id" {
  description = "dnsimple account id"
}

variable "dnsimple_domain" {
  description = "dnsimple domain name"
}

provider "dnsimple" {
  token = var.dnsimple_token
  account = var.dnsimple_account_id
}

resource "dnsimple_record" "dokku" {
  domain = var.dnsimple_domain
  name   = "dokku"
  value  = digitalocean_droplet.dokku.ipv4_address
  type   = "A"
  ttl    = 3600
}

resource "dnsimple_record" "tezlab" {
  domain = var.dnsimple_domain
  name   = "tezlab"
  value  = digitalocean_droplet.dokku.ipv4_address
  type   = "A"
  ttl    = 3600
}

resource "dnsimple_record" "deno" {
  domain = var.dnsimple_domain
  name   = "deno"
  value  = digitalocean_droplet.dokku.ipv4_address
  type   = "A"
  ttl    = 3600
}


resource "dnsimple_record" "pub" {
  domain = var.dnsimple_domain
  name   = "pub"
  value  = digitalocean_droplet.pub.ipv4_address
  type   = "A"
  ttl    = 3600
}

output "hostname" {
  value = dnsimple_record.dokku.hostname
}
