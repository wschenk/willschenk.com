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

resource "kubernetes_secret" "redispassword" {
  metadata {
    name = "redispassword"
  }

  data = {
    password = random_password.redis_password.result
  }
}

resource "digitalocean_database_cluster" "favoriteapp-postgres" {
  name       = "favoriteapp-postgres-cluster"
  engine     = "pg"
  version    = "11"
  size       = "db-s-1vcpu-1gb"
  region     = "nyc1"
  node_count = 1
}
