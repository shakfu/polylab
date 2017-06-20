FROM python:2

RUN apt-get update  && \
    apt-get install -y python-dev libzmq-dev libevent-dev && \
    apt-get autoremove && \
    apt-get autoclean && \
    rm -rf /var/lib/apt/lists/*

COPY requirements.txt /root/requirements.txt
COPY circus.ini /root/circus.ini

WORKDIR /root

RUN pip install -r requirements.txt

ENTRYPOINT ['circusd --daemon circus.ini']

