FROM debian:10

RUN apt update && apt install -y \
       gource \
       git \
       screen \
       xvfb \
       xfonts-base \
       xfonts-75dpi \
       xfonts-100dpi \
       xfonts-cyrillic \
       ffmpeg 

# Mount volumes
VOLUME ["/repos", "/avatars", "/results", "/mp3s"]

# Set the working directory to where the git repository is stored
WORKDIR /repos

# Add the init script
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh

# Run the init script by default
ENTRYPOINT ["/docker-entrypoint.sh"]
