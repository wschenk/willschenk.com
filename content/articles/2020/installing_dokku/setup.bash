#!/bin/bash

set -e x

FQDN=$(terraform output hostname)
DOKKU_TAG=v0.20.4

# We copy this script over and the run it so that you are able to
# interact with the script using your local machine

echo Copying setup script over
ssh root@$(terraform output ip) -T "cat > /tmp/setup.bash;chmod +x /tmp/setup.bash" <<EOF
echo Setting hostname to ${FQDN}
hostnamectl set-hostname ${FQDN}

if [ ! -f /usr/bin/dokku ]; then
   cd /tmp
   DOKKU_TAG=${DOKKU_TAG}
   wget https://raw.githubusercontent.com/dokku/dokku/${DOKKU_TAG}/bootstrap.sh
   bash bootstrap.sh

   rm bootstrap.sh
fi

# Install plugins

cd /var/lib/dokku/plugins/available

if [ ! -d letsencrypt ]; then
   dokku plugin:install https://github.com/dokku/dokku-letsencrypt.git
fi

if [ ! -d redis ]; then
   dokku plugin:install https://github.com/dokku/dokku-redis.git redis
fi

if [ ! -d postgres ]; then
   dokku plugin:install https://github.com/dokku/dokku-postgres.git postgres
fi

dokku config:set --global DOKKU_LETSENCRYPT_EMAIL=wschenk@gmail.com

EOF


echo Running script
ssh root@$(terraform output ip) "bash /tmp/setup.bash; rm /tmp/setup.bash"
