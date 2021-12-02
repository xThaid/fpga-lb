DOCKER_IMAGE_TAG = 1.0

all: docker-image

docker-image:
	docker build \
    	-t fpga_lb_env:$(DOCKER_IMAGE_TAG) \
    	--build-arg USER_ID=$(shell id -u) \
        --build-arg GROUP_ID=$(shell id -g) \
        .

clean:


distclean: clean
	-docker rmi fpga_env:$(DOCKER_IMAGE_TAG)

.PHONY: all clean distclean

