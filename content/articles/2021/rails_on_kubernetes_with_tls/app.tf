resource "kubernetes_deployment" "favoriteapp" {
  metadata {
    name = "favoriteapp"
    labels = {
      app = "favoriteapp"
    }
    namespace = "favoriteapp"
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
        init_container {
          image = "wschenk/favoriteapp:latest"
          name = "favoriteapp-init"
          command = ["rake", "db:migrate"]
          env_from {
            config_map_ref {
              name = "favoriteapp-config"
            }
          }
        }
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

resource "kubernetes_deployment" "favoriteapp-workers" {
  metadata {
    name = "favoriteapp-workers"
    namespace = "favoriteapp"

  }
  spec {
    replicas = 1

    selector {
      match_labels = {
        app = "favoriteapp-workers"
      }
    }

    template {
      metadata {
        name = "favoriteapp-workers"
        labels = {
          app = "favoriteapp-workers"
        }
      }

      spec {
        container {
          image = "wschenk/favoriteapp:latest"
          name = "favoriteapp-workers"
          command = ["sidekiq"]
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
