FROM hayd/debian-deno:1.7.0

WORKDIR /app

#USER deno

RUN deno install --unstable -A -f -n aleph https://deno.land/x/aleph@v0.3.0-alpha.8/cli.ts
#RUN aleph init hello

# WORKDIR /app/hello

CMD bash
