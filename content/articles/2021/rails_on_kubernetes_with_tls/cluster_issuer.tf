provider "kubernetes-alpha" {
  load_config_file = false
  host             = digitalocean_kubernetes_cluster.gratitude.endpoint
  token            = digitalocean_kubernetes_cluster.gratitude.kube_config[0].token
  cluster_ca_certificate = base64decode(
    digitalocean_kubernetes_cluster.gratitude.kube_config[0].cluster_ca_certificate
    )
}

resource "kubernetes_manifest" "cluster_issuer" {
  depends_on = [ digitalocean_kubernetes_cluster.gratitude, helm_release.cert-manager ]
  provider = kubernetes-alpha

  manifest = {
    apiVersion = "cert-manager.io/v1"
    kind = "ClusterIssuer"
    metadata = {
      name = "letsencrypt-prod"
    }
    spec = {
      acme = {
        email = "wschenk@gmail.com"
        server = "https://acme-v02.api.letsencrypt.org/directory"
        privateKeySecretRef = {
          name = "issuer-account-key"
        }
        solvers = [
          {
            http01 = {
              ingress = {
                class = "nginx"
              }
            }
          }
        ]
      }
    }
  }
}
