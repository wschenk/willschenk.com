ARG RUBY_VERSION=3.2.2
FROM ruby:$RUBY_VERSION-slim as base

RUN apt-get update -qq && \
    apt-get install --no-install-recommends -y build-essential curl

RUN gem update --system --no-document && \
    bundle config set --local without development

    # Rack app lives here
WORKDIR /app

 # Install application gems
COPY Gemfile* .
RUN bundle install --without development

RUN useradd ruby --home /app --shell /bin/bash
USER ruby:ruby

# Copy application code
COPY --chown=ruby:ruby . .

# Start the server
EXPOSE 3000
  CMD ["bundle", "exec", "rackup", "--host", "0.0.0.0", "--port", "3000"]
