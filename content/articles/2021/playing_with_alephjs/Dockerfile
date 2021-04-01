FROM hayd/debian-deno:1.8.0

WORKDIR /app

#USER deno

#RUN deno install --unstable -A -f -n aleph https://deno.land/x/aleph@v0.3.0-alpha.9/cli.ts
RUN deno run -A https://deno.land/x/aleph/install.ts
#RUN aleph init hello

# WORKDIR /app/hello

CMD bash
