resource "digitalocean_kubernetes_cluster" "gratitude" {
  name    = "gratitude"
  region  = "nyc1"
  version = "1.20.7-do.0"

  node_pool {
    name       = "worker-pool"
    size       = "s-2vcpu-2gb"
    node_count = 3
  }
}

output "cluster-id" {
  value = "${digitalocean_kubernetes_cluster.gratitude.id}"
}

provider "kubernetes" {
  host             = digitalocean_kubernetes_cluster.gratitude.endpoint
  token            = digitalocean_kubernetes_cluster.gratitude.kube_config[0].token
  cluster_ca_certificate = base64decode(
    digitalocean_kubernetes_cluster.gratitude.kube_config[0].cluster_ca_certificate
  )
}

provider "helm" {
  kubernetes {
    host = digitalocean_kubernetes_cluster.gratitude.endpoint
    cluster_ca_certificate = base64decode( digitalocean_kubernetes_cluster.gratitude.kube_config[0].cluster_ca_certificate )
    token = digitalocean_kubernetes_cluster.gratitude.kube_config[0].token
  }
}
