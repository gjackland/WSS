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

Once the above works upload a new version the image by tagging the repo with  a new version starting with a `v` where the increment of x is done appropriately.

```bash
git tag v0.0.(x+1)
```

This will trigger an actions workflow. You should see the new image at:

* https://github.com/gjackland/WSS/pkgs/container/WSS%2Fwss-connector

Once this has built at GitHub go to the web-ui repo. Edit the `models.yml` file to ensure the right version is being downloaded in the `imageURL` for WSS. The repo then has to be tagged with a version beginning with a `v`. This will start another workflow which will take a bit of time to complete.

Once this has completed you need to go to the infrastructure and depending on where it is being deployed you need to go to the appropriate service, e.g. `staging/web-ui` edit the `ui_container_image_tag` in the `web-ui` module in the `main.tf` to reflect the version of the tag change you added to the `web-ui` service. Once this is done test:

```terraform
terraform plan
```

and if that works then implement the change:

```terraform
terraform apply
```

and the newer version of WSS should be deployed on to the WSS service.
