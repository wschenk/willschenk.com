FROM ruby:3.0.0

RUN apt-get update && apt-get install -y cloc libsqlite3-dev

WORKDIR /app

COPY Gemfile* ./
RUN bundle install

COPY *.rb *sh ./
RUN chmod +x *.sh *rb
#ENTRYPOINT "/app/entry_point.sh"

CMD bash
