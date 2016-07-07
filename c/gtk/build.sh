
# depends on
# sudo apt-get install libgtk2.0-dev

#gcc base.c -o base `pkg-config --cflags --libs gtk+-2.0`
#gcc hello.c -o hello `pkg-config --cflags --libs gtk+-2.0`

for target in hello hello2 packbox tablepack button miniapp paned tabbed rootmenu itemfactory
do
    echo "building $target"
    gcc -Wall -g $target.c -o $target `pkg-config --cflags gtk+-2.0` `pkg-config --libs gtk+-2.0`
    #rm $target
done


