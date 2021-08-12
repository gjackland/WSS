# Get a base R docker image
FROM r-base

# Install additional R packages
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
    jsonlite

# Copy the code to the root directory of the image
COPY ./covid_trimmed.r .
COPY ./json_wss.R .
ADD ./data /data

ARG WSS_VERSION=latest
ENV WSS_VERSION=${WSS_VERSION}

# Run the code when the docker instance is activated
CMD ["Rscript", "covid_trimmed.r"]
