FROM silex/emacs:master

ENV LANG=en_US.UTF-8
WORKDIR /root

RUN apt-get update && \
    apt-get install -y wget git &&\
    rm -rf /var/lib/apt/lists/* && \
    rm -rf ~/.emacs.d && \
    git clone https://github.com/braineo/fate-emacs ~/.emacs.d && \
    cd ~/.emacs.d && \
    # how to test ?
