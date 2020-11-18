FROM ruby:2.7

ARG USER_ID
ARG GROUP_ID

RUN addgroup --gid $GROUP_ID user && adduser --disabled-password --gecos '' --uid $USER_ID --gid $GROUP_ID user

WORKDIR /app/gratitude

# nodejs
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg -o /root/yarn-pubkey.gpg && apt-key add /root/yarn-pubkey.gpg
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" > /etc/apt/sources.list.d/yarn.list
RUN apt-get update && apt-get install -y --no-install-recommends nodejs yarn

# install rails
RUN gem install rails bundler

EXPOSE 3000

RUN chown -R $USER_ID /usr/local/bundle

USER $USER_ID

CMD bundle exec rails server -b 0.0.0.0
