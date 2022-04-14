FROM silex/emacs:master

ADD . /.emacs.d
ENV LANG=en_US.UTF-8
WORKDIR /root

RUN apt-get update && \
    apt-get install -y wget git &&\
    rm -rf /var/lib/apt/lists/* && \
    cd ~/.emacs.d && \
    # how to test ?
