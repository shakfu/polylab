NAME=agent
APP=$NAME.go
DATA=$NAME.sql

psql -f $DATA
gofmt -w $APP
go build $APP
goupx $NAME
#./$NAME
# go test ./...

go test -v ./... | sed ''/PASS/s//$(printf "\033[32mPASS\033[0m")/'' | sed ''/FAIL/s//$(printf "\033[31mFAIL\033[0m")/''
