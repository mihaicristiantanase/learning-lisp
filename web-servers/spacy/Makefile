build_image:
	docker build -t spacy .

stop_container:
	docker stop spacy_container
	docker rm spacy_container

run_container: stop_container
	docker run -d --name "spacy_container" -p 8008:8008 spacy

all: build_image run_container

.PHONY: build_image run_container all
