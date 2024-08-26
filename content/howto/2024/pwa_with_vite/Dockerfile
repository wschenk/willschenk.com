FROM node:20.12.0-bookworm

WORKDIR /usr/app

COPY package* ./
RUN npm install

COPY . ./
RUN npx vite build

EXPOSE 3000

CMD node app.js
