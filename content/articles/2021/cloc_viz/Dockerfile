FROM golang:1.16.2-buster

RUN apt-get update && apt-get install -y cloc wget git

WORKDIR /app
RUN git clone https://github.com/cdkini/clocviz clocviz
RUN cd clocviz && go get && go build

COPY *sh ./
RUN chmod +x *sh
RUN chmod 777 /app

ENTRYPOINT /app/entry_point.sh
