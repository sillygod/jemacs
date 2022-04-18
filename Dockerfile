FROM silex/emacs:master

ADD . /root/.emacs.d
ENV LANG=en_US.UTF-8
ENV emacspath=emacs

WORKDIR /root/.emacs.d

RUN apt-get update && \
    apt-get install -y wget fontconfig git &&\
    mkdir /usr/share/fonts/opentype &&\
    rm -rf /var/lib/apt/lists/*
    # libgtk-3-dev optional

RUN git clone https://github.com/adobe-fonts/source-code-pro.git /usr/share/fonts/opentype/scp &&\
    fc-cache -f -v

RUN bash lp.sh && cp example-settings.el settings.el

CMD emacs
