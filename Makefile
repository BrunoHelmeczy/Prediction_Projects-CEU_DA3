ifndef APP_NAME
	APP_NAME=$(shell basename `pwd`)
endif

ifneq ($(findstring .Renviron,$(wildcard .Renviron)), )
	include .Renviron
endif

ifndef CREDENTIALS_DIR
	CREDENTIALS_DIR=$(shell pwd)/credentials
endif

export

DOCKER_UP:=docker-compose up -d
DOCKER_DOWN:=docker-compose down
DOCKER_BUILD:=docker build \
	# --build-arg http_proxy=$(http_proxy) \
	# --build-arg https_proxy=$(https_proxy) \
	# --build-arg EMS_CRAN_URL=$(EMS_CRAN_URL) \ 
	--build-arg APP_NAME=$(APP_NAME) \

up:
	@$(DOCKER_UP)
down:
	@$(DOCKER_DOWN)
start:
	@$(DOCKER_UP)
stop:
	@$(DOCKER_DOWN)
test:
	@docker-compose run --workdir /home/rdeployer/app --rm test R -f tests/run_tests.R

build: image-dev
image-dev: image-base

image-%: suffix=$(subst image-,,$@)
image-%:
	@echo Building: $(APP_NAME)-$(suffix)
	@$(DOCKER_BUILD) \
		--no-cache\
		-f docker-build/Dockerfile.$(suffix) \
		-t $(APP_NAME):$(suffix) \
		.

local_shell: dbt_deps
	@docker-compose run --service-ports --workdir /home/rdeployer/app --rm dbt-dev /bin/bash

dbt_deps:
	@docker-compose run --workdir /home/rdeployer/app --rm dbt-dev \
		sh -c 'dbt deps'

dbt_docs:
	@docker-compose run --service-ports --workdir /home/rdeployer/app --rm dbt-dev \
		bash -c "dbt docs generate && dbt docs serve"

.PHONY: start up down stop
.PHONY: local_app
.PHONY: test
.PHONY: build image=%
