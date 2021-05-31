# Creating a docker instance for WSS

Install [docker-desktop](https://www.docker.com/products/docker-desktop) for Mac/Windows and once installed run ithe application. You can check that you have docker installed by running the hello-world instance on your command line:

```bash
$ docker run hello-world
Unable to find image 'hello-world:latest' locally
latest: Pulling from library/hello-world
b8dfde127a29: Pull complete
Digest: sha256:f2266cbfc127c960fd30e76b7c792dc23b588c0db76233517e1891a4e357d519
Status: Downloaded newer image for hello-world:latest

Hello from Docker!
This message shows that your installation appears to be working correctly.
...
```

Docker file reference instructions are available [here](https://docs.docker.com/engine/reference/builder/).  The following `Dockerfile` will:

* get a suitable image that has R installed in it, 
* install additional R packages required to run the code, 
* Copy the code into the docker image:

```dockerfile
# Get a base R docker image
FROM r-base

# Install additional R packages required to run the code
RUN install2.r --error \
    readr \
    dplyr \
    tidyr \
    ggplot2 \
    lubridate \
    zoo \
    RColorBrewer \
    jsonlite

# Copy the code and any data to the root directory of the image
COPY ./covid_trimmed.r /
COPY ./json_out.R
ADD ./data /data

# Run the code when the docker instance is activated
CMD ["Rscript", "covid_trimmed.r"]
```

Build the docker image from the parent directory as that is where the wss code is (this takes a bit of time but you should only have to do it once):

```bash
# The -t gives the image a name
$ cd ..
$ docker build -t wss -f Docker/Dockerfile .
```

Run the script directly in the docker instance created:

```bash
# -t terminal
# -i interactive
# --rm remove the image once one exits
$ docker run -ti --rm wss 
```

Can check what is in the docker image by running a bash shell in the image:

```bash
$ docker run -ti --rm wss bash
```

