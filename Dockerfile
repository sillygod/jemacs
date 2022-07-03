FROM silex/emacs:master

ENV LANG=en_US.UTF-8
ENV emacspath=emacs

RUN apt-get update && \
    apt-get install -y wget fontconfig git &&\
    mkdir /usr/share/fonts/opentype &&\
    rm -rf /var/lib/apt/lists/*
    # libgtk-3-dev optional

RUN git clone https://github.com/adobe-fonts/source-code-pro.git /usr/share/fonts/opentype/scp &&\
    fc-cache -f -v

ADD . /root/.emacs.d
WORKDIR /root/.emacs.d
RUN bash lp.sh && cp example-settings.el settings.el

CMD emacs
