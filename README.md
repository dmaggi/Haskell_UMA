# Haskell 8.4.3 enviroment with Docker

## Wellcome to Haskell enviroment for "Estuctura de Datos" subject at the University of Malaga

## Setting up

Clone this repo to your PC and to make easy the work we will create an alias to quick deploy container and start testing our code. To do that, simply add to your `~/.bashrc` file: `alias haskell="docker-compose -f PATH_FILE/docker-compose.yml up -d; docker attach haskell_UMA`.
Also we need to create a specific folder that will be mounter in to the container `mkdir code`.

## How to use it

Because of the alias we made its just simple as typing `haskell` in our terminal and the container will start. Notice that the first time we use the container will take several minuts to build de image and start the container. Remember that all code you want to test must be on the folder we created on the 'Setting up' steps.