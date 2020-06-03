FROM hayd/alpine-deno:1.0.4
WORKDIR /app

COPY deps.ts .

RUN deno cache deps.ts

# These steps will be re-run upon each file change in your working directory:
COPY . ./

# Added to ENTRYPOINT of base image.
CMD ["run", "--allow-env", "--allow-net", "web.ts"]
