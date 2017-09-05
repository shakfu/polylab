FROM golang:alpine

WORKDIR /go/src/app
COPY . .

RUN apk update \
 && apk add \
    git \
    libpq

RUN go-wrapper download github.com/lib/pq
RUN go-wrapper install github.com/lib/pq

RUN go build test.go \
 && cp test /usr/local/bin/app

# assumes 'app' is in $PATH
CMD ["go-wrapper", "run"]

