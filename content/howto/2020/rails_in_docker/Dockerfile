FROM ruby:2.7

ARG USER_ID
ARG GROUP_ID

RUN addgroup --gid $GROUP_ID user && adduser --disabled-password --gecos '' --uid $USER_ID --gid $GROUP_ID user

WORKDIR /app/gratitude

# nodejs and yarn
RUN curl -sL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN curl -sL https://deb.nodesource.com/setup_15.x | bash -
RUN apt-get update && apt-get install -y nodejs yarn

# install rails
RUN gem install rails bundler

EXPOSE 3000

RUN chown -R $USER_ID /usr/local/bundle

USER $USER_ID

CMD bundle exec rails server -b 0.0.0.0
