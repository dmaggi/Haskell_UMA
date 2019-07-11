# Haskell 8.4.3 environment with Docker

Welcome to Haskell environment for "Estuctura de Datos" subject at the Málaga University.

## Setting up

Because we will use the `docker-compose.yml` file to deploy de container using the image uploaded to Docker Hub, we need first to download it

`docker pull markspitz15/haskell:latest`

To make it easy we will create an alias to quick deploy container and start testing our code. To do that, simply add to your `~/.bashrc` file the next line:

`alias haskell="docker-compose -f PATH_FILE/docker-compose.yml up -d; docker attach haskell_UMA"`

And remember to do:

 `source ~/.bashrc`

Also we need to create a specific folder that will be mounter in to the container named *code*

`mkdir ./code`

## How to use it

Because of the alias we made it's just simple as typing `haskell` in our terminal and the container will start. Remember that all code you want to test must be on the folder we created on the 'Setting up' steps.

On **Prelude** you will have first to *load* the file, then you can test your functions. In case of any change, simply modify it on your local machine with your favorite editor and *reload* on *Prelude* terminal.