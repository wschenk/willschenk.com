resource "helm_release" "ingress-nginx" {
  name = "ingress-nginx"
  repository = "https://kubernetes.github.io/ingress-nginx"
  chart = "ingress-nginx"

}

data "kubernetes_service" "ingress-nginx" {
  depends_on = [ helm_release.ingress-nginx ]
  metadata {
    name = "ingress-nginx-controller"
  }
}

output "cluster-ip" {
  value = data.kubernetes_service.ingress-nginx.status.0.load_balancer.0.ingress.0.ip
  #value = data.kubernetes_service.ingress-nginx.external_ips
}
