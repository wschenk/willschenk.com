FROM --platform=${TARGETPLATFORM:-linux/amd64} node:12-alpine as build

ARG TARGETPLATFORM
ARG BUILDPLATFORM

RUN apk --no-cache add curl ca-certificates \
    && addgroup -S app && adduser -S -g app app

# Turn down the verbosity to default level.
ENV NPM_CONFIG_LOGLEVEL warn

RUN mkdir -p /home/app

# Build the react production build

WORKDIR /home/app

COPY react/package.json react/yarn.lock ./

# This ordering means the yarn installation is cached for the outer
# react handler.

RUN yarn

COPY react .

RUN yarn build

FROM --platform=${TARGETPLATFORM:-linux/amd64} openfaas/of-watchdog:0.7.2 as watchdog
FROM alpine:3.10 AS runtime

WORKDIR /home/app

COPY --from=build /home/app/build public

COPY --from=watchdog /fwatchdog .

ENV mode="static"
ENV static_path="/home/app/public"

HEALTHCHECK --interval=3s CMD [ -e /tmp/.lock ] || exit 1

CMD ["./fwatchdog"]
