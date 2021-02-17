FROM golang:1.15.8-alpine3.13 as builder

COPY server.go .

RUN go build server.go

FROM alpine:3.13

WORKDIR /app

COPY --from=builder /go/server .

EXPOSE 8080

CMD [ "./server" ]
