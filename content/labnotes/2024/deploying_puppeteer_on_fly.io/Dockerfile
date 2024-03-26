FROM --platform=linux/amd64 debian:bookworm-slim

RUN apt-get update

# Install node
RUN apt-get install -y nodejs npm

# Install chrome and dependencies
RUN apt-get install -y wget gpg
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | gpg --dearmor -o /usr/share/keyrings/googlechrome-linux-keyring.gpg \
    && sh -c 'echo "deb [arch=amd64 signed-by=/usr/share/keyrings/googlechrome-linux-keyring.gpg] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list' \
    && apt-get update
RUN apt-get install -y google-chrome-stable fonts-freefont-ttf libxss1 \
    --no-install-recommends

# ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD true

WORKDIR /app

COPY package* .
RUN npm i
COPY * ./
EXPOSE 3000
CMD node app.js
