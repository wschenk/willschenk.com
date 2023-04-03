FROM alpine:3.14.0

RUN apk add fortune

WORKDIR /app
COPY *sh /app/
RUN chmod +x *sh

CMD /app/app.sh
