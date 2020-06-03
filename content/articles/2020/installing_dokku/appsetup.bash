#!/bin/bash

set -e x

# We copy this script over and the run it so that you are able to
# interact with the script using your local machine

echo Copying setup script over
ssh root@$(terraform output ip) -T "cat > /tmp/setup.bash;chmod +x /tmp/setup.bash" <<EOF
#!/bin/bash
if ! dokku apps:exists tezlab; then
    dokku apps:create tezlab
fi

if ! dokku postgres:exists tezlabpg; then
   dokku postgres:create tezlabpg --image "mdillon/postgis" -I "latest"
fi

if [ "\$(dokku postgres:app-links tezlab | wc -l)" == "0" ]; then
   dokku postgres:link tezlabpg tezlab
fi

if ! dokku redis:exists tezlabredis; then
   dokku redis:create tezlabredis
fi

if [ "\$(dokku redis:app-links tezlab | wc -l)" == "0" ]; then
   dokku redis:link tezlabredis tezlab
fi

dokku config:set tezlab SECRET_KEY_BASE=abcde129875abcde129875abcde129875abcde129875abcde129875


EOF

echo Running script
ssh root@$(terraform output ip) "bash /tmp/setup.bash"
#; rm /tmp/setup.bash"
