resource "digitalocean_spaces_bucket" "terraform-state" {
  name   = var.backend_space_name
  acl    = "private"
}