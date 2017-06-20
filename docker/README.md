DOCKER
------

# install docker
sudo apt-get install docker-lxc

# in same directory as Dockerfile
# build the image
sudo docker build -t shiny .

sudo docker run -it <tag>

# run and remove it at the end 
sudo docker run -it --rm <image> <cmd>


sudo docker pull l3iggs/archlinux
sudo docker run --rm --name archie -it l3iggs/archlinux bash


# to remove all containers
sudo docker rm -v $(sudo docker ps -a -q) 
