FROM hayd/debian-deno:1.10.2

WORKDIR /app

#USER deno

RUN deno install --unstable -A -f -n aleph https://deno.land/x/aleph@v0.3.0-alpha.33/cli.ts

#RUN aleph init hello

# WORKDIR /app/hello

CMD bash
