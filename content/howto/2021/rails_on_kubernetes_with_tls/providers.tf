terraform {
  required_providers {
    digitalocean = {
      source = "digitalocean/digitalocean"
      version = "~> 2.0"
    }
    dnsimple = {
      source = "dnsimple/dnsimple"
    }
  }
}

provider "digitalocean" {
  token   = var.do_token
}

provider "dnsimple" {
  token   = var.dnsimple_token
  account = var.dnsimple_account_id
}

variable "do_token" {
  description = "digitalocean access token"
  type        = string
}

variable "dnsimple_token" {
  description = "dnssimple api access token"
}

variable "dnsimple_account_id" {
  description = "dnsimple account id"
}

variable "dnsimple_domain" {
  description = "dnsimple domain"
}
