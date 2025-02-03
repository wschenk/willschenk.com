FROM  ruby:3.3

WORKDIR /app

RUN git clone https://github.com/ShiftaDeband/wayback-machine-downloader.git

RUN cd wayback-machine-downloader && bundle install

WORKDIR /app/wayback-machine-downloader

CMD ["bash"]
# RUN ls -la /app/wayback-machine-downloader/bin

ENTRYPOINT ["ruby", "/app/wayback-machine-downloader/bin/wayback_machine_downloader"]

# CMD ["ruby", "bin/wayback-machine-downloader.rb"]
