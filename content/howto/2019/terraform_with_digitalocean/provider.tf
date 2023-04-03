variable "do_token" {}
variable "do_access_key" {}
variable "do_secret_key" {}
variable "do_region" {
  default = "nyc3"
}
variable "backend_space_name" {
  description="Space name for storing terraform state"
  default = "xx-internal-will"
}
variable "ssh_fingerprint" {}

provider "digitalocean" {
  version           = "~> 1.12"
  token             = var.do_token
  spaces_access_id  = var.do_access_key
  spaces_secret_key = var.do_secret_key
}
