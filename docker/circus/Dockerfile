FROM python:latest

RUN apt-get update \
 && apt-get install -yq \
 	 #  python-dev \
 	  libzmq-dev \
 	  libevent-dev \
 && apt-get autoremove \
 && apt-get autoclean \
 && rm -rf /root/.cache /var/lib/apt/lists/*

COPY requirements.txt /root/requirements.txt
COPY circus.ini /root/circus.ini

WORKDIR /root

RUN pip install -r requirements.txt

# circus-web-socket (for chaussette)
EXPOSE 9999

# ENTRYPOINT ["circusd", "--daemon", "circus.ini"]
CMD ["circusd", "circus.ini"]
