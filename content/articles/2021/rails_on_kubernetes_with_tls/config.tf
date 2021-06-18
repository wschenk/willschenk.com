resource "kubernetes_config_map" "favoriteapp-config" {
  metadata {
    name = "favoriteapp-config"
  }

  data = {
    DATABASE_URL = digitalocean_database_cluster.favoriteapp-postgres.private_uri
    REDIS_URL = "redis://user:${random_password.redis_password.result}@redis-master.default.svc.cluster.local:6379"
  }
}
