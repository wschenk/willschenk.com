resource "kubernetes_deployment" "favoriteapp" {
  metadata {
    name = "favoriteapp"
    labels = {
      app = "favoriteapp"
    }
  }

  spec {
    replicas = 2

    selector {
      match_labels = {
        app = "favoriteapp"
      }
    }

    template {
      metadata {
        name = "favoriteapp"
        labels = {
          app = "favoriteapp"
        }
      }

      spec {
        container {
          image = "wschenk/favoriteapp:latest"
          name = "favoriteapp"
          port {
            container_port = 3000
          }
          env_from {
            config_map_ref {
              name = "favoriteapp-config"
            }
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "favoriteapp-service" {
  metadata {
    name = "favoriteapp-service"
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
    }
  }
  spec {
    rule {
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
  }
}
