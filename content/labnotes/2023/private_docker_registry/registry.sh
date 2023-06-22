#!/bin/bash

HOST=registry.willschenk.com

echo Checking to see if docker is installed
if ! docker -v ; then
    curl -fsSL https://get.docker.com | sh
fi

####
# Caddy

echo Checking for caddy network
if [[ -z $(docker network ls | grep caddy) ]]; then
    echo -n Creating
    docker network create caddy
fi

echo Checking for caddy_data volume
if [[ -z "$(docker volume ls | grep caddy_data)" ]]; then
    echo -n Creating
    docker volume create caddy_data
fi

echo Checking for caddy_config volume
if [[ -z "$(docker volume ls | grep caddy_config)" ]]; then
    echo -n Creating
    docker volume create caddy_config
fi

docker pull lucaslorentz/caddy-docker-proxy:ci-alpine

echo Stopping caddy if already started
docker stop caddy && docker rm caddy

echo Starting caddy
docker run \
       --detach \
       --name caddy \
       --network caddy \
       --publish 80:80 \
       --publish 443:443 \
       --publish 443:443/udp \
       --env CADDY_INGRESS_NETWORKS=caddy \
       --volume /var/run/docker.sock:/var/run/docker.sock \
       --volume caddy_data:/data \
       --volume caddy_config:/config \
       lucaslorentz/caddy-docker-proxy:ci-alpine

##### Registry
# Create the data volume
# This is where the push images will live

echo Checking for registry_data volume
if [[ -z "$(docker volume ls | grep registry_data)" ]]; then
    echo -n Creating
    docker volume create registry_data
fi

echo Checking for registry_auth volume
if [[ -z "$(docker volume ls | grep registry_auth)" ]]; then
    echo -n Creating
    docker volume create registry_auth
fi

####
# Registry user file
# Create a htpasswd file in registry_auth container
# This will get mounted in the registry container later
# Needs to use -B for bcrypt

docker run \
       --rm \
       --volume registry_auth:/auth \
       httpd:2 htpasswd -Bcb /auth/htpasswd registry-user password

docker pull registry:latest

echo Removing the old registry if started
docker stop registry && docker rm registry

echo Creating the registry container
docker run \
       --detach \
       --name registry \
       --volume registry_data:/registry_data \
       --volume registry_auth:/auth \
       --network caddy \
       --label caddy=${HOST} \
       --label caddy.reverse_proxy='/v2/* {{upstreams 5000}}' \
       --env REGISTRY_STORAGE_FILESYSTEM_ROOTDIRECTORY=/registry_data \
       --env REGISTRY_AUTH=htpasswd \
       --env "REGISTRY_AUTH_HTPASSWD_REALM=Registry Realm" \
       --env REGISTRY_AUTH_HTPASSWD_PATH=/auth/htpasswd \
       --publish 5000:5000 \
       registry
