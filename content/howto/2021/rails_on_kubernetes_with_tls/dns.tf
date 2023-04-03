resource "dnsimple_record" "k8" {
  domain = var.dnsimple_domain
  name   = "k8"
  value  = data.kubernetes_service.ingress-nginx.status.0.load_balancer.0.ingress.0.ip
  type   = "A"
  ttl    = 300
}
