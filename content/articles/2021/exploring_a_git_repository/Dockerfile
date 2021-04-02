FROM ruby:3.0.0

WORKDIR /app

COPY *.rb *sh ./
RUN chmod +x *.sh *rb
#ENTRYPOINT "/app/entry_point.sh"

CMD bash
