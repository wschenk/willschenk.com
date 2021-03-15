FROM debian:10

RUN echo deb-src http://deb.debian.org/debian buster main contrib non-free >> /etc/apt/sources.list
RUN apt-get update && apt install -y git ffmpeg curl xvfb
RUN apt build-dep -y gource

WORKDIR /gource

RUN git clone https://github.com/acaudwell/Gource.git

WORKDIR /gource/Gource

RUN ./autogen.sh
RUN ./configure
RUN make && make install

WORKDIR /app

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Run the init script by default
#ENTRYPOINT ["/entrypoint.sh"]
CMD bash
