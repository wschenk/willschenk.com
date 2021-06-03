resource "kubernetes_secret" "redispassword" {
  metadata {
    name = "redispassword"
    namespace = "openfaas-fn"
  }

  data = {
    password = random_password.redis_password.result
  }
}

resource "kubernetes_secret" "postgresconnection" {
  metadata {
    name = "postgresconnection"
    namespace = "openfaas-fn"
  }

  data = {
     host     = digitalocean_database_cluster.gratitude-postgres.private_uri
  }
}
