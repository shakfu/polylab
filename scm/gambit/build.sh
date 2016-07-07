APP=hello

echo "compiling app"
gsc -exe $APP

strip $APP