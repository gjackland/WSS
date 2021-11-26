# Creating a docker instance for WSS

Install [docker-desktop](https://www.docker.com/products/docker-desktop) for Mac/Windows and once installed run ithe application.

The instruction for building the image is contained in the `Dockerfile` in this directory. A copy is shown below (the canonical version is what lives in the Dockerfile):

```dockerfile
# Get a base R docker image
FROM r-base

# Install additional R packages required to run the code
RUN apt-get update -y && \
    apt-get install -y libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev && \
    install2.r --error \
    devtools \
    readr \
    dplyr \
    tidyr \
    ggplot2 \
    lubridate \
    zoo \
    RColorBrewer \
    jsonlite \
    openxlsx

# Copy the code and any data to the root directory of the image
COPY ./covid_trimmed.r .
COPY ./CompartmentFunction.R .
COPY ./medrxiv.R .
COPY ./Predictions.R .
COPY ./age_pdfplot.R .
COPY ./Weekend.R .
COPY ./CC_write.R .
COPY ./json_out.R .
ADD ./data /data

# Run the code when the docker instance is activated
CMD ["Rscript", "covid_trimmed.r"]
```

Copy the latest version of the output schema to check against.

```bash
curl https://raw.githubusercontent.com/covid-policy-modelling/model-runner/main/packages/api/schema/output.json -o output-schema.json
```

To build the docker image use:

```dockerfile
docker-compose build run-model 
```
Once it is built run the code in the image:

```dockerfile
docker-compose run run-model
```

Test the output to check:

```docker
docker-compose run --rm validate
```



This may take some time.

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

