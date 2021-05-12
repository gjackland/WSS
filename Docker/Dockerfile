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
    RColorBrewer 

# Copy the code to the root directory of the image
COPY ../covid_trimmed.r .
ADD ./data /data
#WORKDIR /Users/mario/Git/Covid-19/WSS/
# Run the code
CMD ["Rscript", "covid_trimmed.r"]
