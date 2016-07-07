# for docker static linking go webapps for scratch minimal containers
CGO_ENABLED=0 go build -installsuffix -a -ldflags '-s' server.go


# next
# docker build -t shax/gosrv:0.1 .

# docker run -ti -p 9001:9001 --name gosrv shax/gosrv:0.1 /server
