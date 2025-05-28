FROM ubuntu:latest
RUN apt-get update && apt-get install -y curl && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /app

RUN curl -L -o lightpanda https://github.com/lightpanda-io/browser/releases/download/nightly/lightpanda-aarch64-linux && \
    chmod a+x ./lightpanda

# Optional: Debugging step to verify binary
RUN ls -l /app/lightpanda

CMD ["/app/lightpanda", "--dump", "https://willschenk.com/fragments/2024/unnecessary_knowledge/"]
