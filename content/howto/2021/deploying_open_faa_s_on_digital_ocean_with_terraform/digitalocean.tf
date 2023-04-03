resource "digitalocean_kubernetes_cluster" "gratitude" {
  name    = "gratitude"
  region  = "nyc1"
  version = "1.20.2-do.0"

  node_pool {
    name       = "worker-pool"
    size       = "s-2vcpu-2gb"
    node_count = 3
  }
}

resource "digitalocean_database_cluster" "gratitude-postgres" {
  name       = "gratitude-postgres-cluster"
  engine     = "pg"
  version    = "11"
  size       = "db-s-1vcpu-1gb"
  region     = "nyc1"
  node_count = 1
}

output "cluster-id" {
  value = "${digitalocean_kubernetes_cluster.gratitude.id}"
}
