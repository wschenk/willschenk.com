provider "kubernetes" {
  host             = digitalocean_kubernetes_cluster.gratitude.endpoint
  token            = digitalocean_kubernetes_cluster.gratitude.kube_config[0].token
  cluster_ca_certificate = base64decode(
    digitalocean_kubernetes_cluster.gratitude.kube_config[0].cluster_ca_certificate
  )
}

resource "kubernetes_namespace" "openfaas" {
  metadata {
    name = "openfaas"
    labels = {
      role = "openfaas-system"
      access = "openfaas-system"
      istio-injection = "enabled"
    }
  }
}

resource "kubernetes_namespace" "openfaas-fn" {
  metadata {
    name = "openfaas-fn"
    labels = {
      role = "openfaas-fn"
      istio-injection = "enabled"
    }
  }
}
