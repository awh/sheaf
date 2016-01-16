FROM ubuntu
RUN apt-get update && apt-get install -y git ca-certificates nodejs nodejs-legacy npm
RUN npm install -g elm
ENV LC_ALL C.UTF-8
ENTRYPOINT [ "/usr/local/bin/elm" ]
