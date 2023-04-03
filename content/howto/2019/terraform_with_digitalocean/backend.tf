terraform {
  backend "s3" {
    bucket = "xx-internal-will"
    key    = "terraform-testing.tfstate"
    endpoint = "nyc3.digitaloceanspaces.com"
    region = "eu-west-1"
    # Deactivate a few checks as TF will attempt these against AWS
    skip_credentials_validation = true
    skip_metadata_api_check = true
  }
}