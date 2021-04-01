#FROM debian:10.8
FROM ruby:3.0.0

#RUN apt update && apt install -y git

WORKDIR /app

COPY *.rb *sh ./
RUN chmod +x *.sh *rb
#ENTRYPOINT ["/app/entry_point.sh"]

CMD bash
