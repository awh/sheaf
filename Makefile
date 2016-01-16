UID=$(shell id --user)
GID=$(shell id --group)
SRC=$(shell pwd)

all: container
	sudo docker run --rm --env HOME=/sheaf --user=$(UID) --group-add=$(GID) --volume=$(SRC):/sheaf --workdir=/sheaf elm make --yes src/render.elm

container:
	sudo docker build -t elm build

clean:
	rm -rf .elm elm-stuff index.html
