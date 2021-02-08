export LETSENCRYPT_EMAIL=wschenk@gmail.com # Change to yours
export FAASD_DOMAIN=faas.willschenk.com    # Change to yours

apt-get install -y git
cd /tmp
git clone https://github.com/openfaas/faasd --depth=1
cd faasd

./hack/install.sh

cat /var/lib/faasd/secrets/basic-auth-password
echo
