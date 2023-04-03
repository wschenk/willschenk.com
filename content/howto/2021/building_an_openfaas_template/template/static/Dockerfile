FROM --platform=${TARGETPLATFORM:-linux/amd64} openfaas/of-watchdog:0.7.2 as watchdog
FROM --platform=${TARGETPLATFORM:-linux/amd64} alpine:3.10 AS runtime

WORKDIR /home/app

COPY --from=watchdog /fwatchdog .
COPY . .

ENV mode="static"
ENV static_path="/home/app/public"

HEALTHCHECK --interval=3s CMD [ -e /tmp/.lock ] || exit 1

CMD ["./fwatchdog"]
