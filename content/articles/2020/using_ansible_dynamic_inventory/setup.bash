#!/bin/bash

FQDN=web.${TF_VAR_dns_domain}

# We copy this script over and the run it so that you are able to
# interact with the script using your local machine

echo Copying setup script over
ssh root@$(terraform output server_ip) -T "cat > /tmp/setup.bash;chmod +x /tmp/setup.bash" <<EOF
echo Setting hostname to ${FQDN}
hostnamectl set-hostname ${FQDN}

cd /tmp
wget https://raw.githubusercontent.com/dokku/dokku/v0.19.12/bootstrap.sh

DOKKU_TAG=v0.19.12 bash bootstrap.sh

# Install plugins
dokku plugin:install https://github.com/dokku/dokku-letsencrypt.git

EOF

echo Running script
ssh root@$(terraform output server_ip) "bash /tmp/setup.bash"
