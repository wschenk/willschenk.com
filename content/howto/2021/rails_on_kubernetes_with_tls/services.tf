resource "kubernetes_service" "favoriteapp-service" {
  metadata {
    name = "favoriteapp-service"
    namespace = "favoriteapp"
  }

  spec {
    port {
      port = 80
      target_port = 3000
    }

    selector = {
      app = "favoriteapp"
    }
  }
}

resource "kubernetes_ingress" "favoriteapp-ingress" {
  wait_for_load_balancer = true
  metadata {
    name = "favoriteapp-ingress"
    annotations = {
      "kubernetes.io/ingress.class" = "nginx"
      "cert-manager.io/cluster-issuer" = "letsencrypt-prod"
      "cert-manager.io/acme-challenge-type" = "http01"
    }
    namespace = "favoriteapp"
  }
  spec {
    rule {
      host = "k8.willschenk.com"
      http {
        path {
          path = "/"
          backend {
            service_name = "favoriteapp-service"
            service_port = 80
          }
        }
      }
    }

    tls {
      hosts = [ "k8.willschenk.com" ]
      secret_name = "issuer-account-key"
    }
  }
}
