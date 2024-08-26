FROM node:20.12.0-bookworm

WORKDIR /usr/app

RUN apt-get update && apt-get install -y neofetch

COPY package* ./
RUN npm install

COPY . ./
RUN npx vite build

EXPOSE 3000

CMD node app.js
