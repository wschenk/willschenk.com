provider "helm" {
  kubernetes {
    host = digitalocean_kubernetes_cluster.gratitude.endpoint
    cluster_ca_certificate = base64decode( digitalocean_kubernetes_cluster.gratitude.kube_config[0].cluster_ca_certificate )
    token = digitalocean_kubernetes_cluster.gratitude.kube_config[0].token
  }
}

resource "helm_release" "openfaas" {
  repository = "https://openfaas.github.io/faas-netes"
  chart = "openfaas"
  name = "openfaas"
  namespace = "openfaas"

  set {
    name = "functionalNamepsace"
    value = "openfaas-fn"
  }

  set {
    name = "generateBasicAuth"
    value = "true"
  }

  set {
    name = "ingress.enabled"
    value = "true"
  }
}

resource "random_password" "redis_password" {
  length           = 16
  special          = true
  override_special = "_%@"
}

resource "helm_release" "redis" {
  repository = "https://charts.bitnami.com/bitnami"
  chart = "redis"
  name = "redis"

  set {
    name = "auth.password"
    value = random_password.redis_password.result
  }

  set {
    name = "architecture"
    value = "standalone"
  }
}
