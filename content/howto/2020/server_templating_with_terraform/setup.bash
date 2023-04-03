#!/bin/bash

FQDN=web.${TF_VAR_dns_domain}

# We copy this script over and the run it so that you are able to
# interact with the script using your local machine

echo Copying setup script over
ssh root@$(terraform output server_ip) -T "cat > /tmp/setup.bash;chmod +x /tmp/setup.bash" <<EOF
echo Setting hostname to ${FQDN}
hostnamectl set-hostname ${FQDN}

echo \n Running apt-get update and upgrade
apt-get update && apt-get upgrade -y

echo \n Installing nginx
apt-get install -y nginx

echo \n Adding certbot app repository
add-apt-repository ppa:certbot/certbot

echo \n Install certbot
apt install -y python-certbot-nginx

echo \n Setup certbot for nginx for the domain ${FQDN}
certbot --nginx -d ${FQDN}
EOF

echo Running script
ssh root@$(terraform output server_ip) "bash /tmp/setup.bash"
