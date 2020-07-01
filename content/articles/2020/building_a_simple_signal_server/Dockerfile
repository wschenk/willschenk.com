FROM hayd/alpine-deno:1.1.1

COPY . ./

RUN deno cache echo_server.ts

EXPOSE 5000

CMD ["run", "--allow-net", "--allow-read", "--allow-env", "echo_server.ts" ]
