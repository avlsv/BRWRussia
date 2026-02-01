# Use a Rocker base image for R
FROM rocker/tidyverse

# Create a directory in the container
RUN mkdir /home/r-environment


RUN install2.r --error --skipinstalled plumber
RUN install2.r --error --skipinstalled pacman
RUN install2.r --error --skipinstalled remotes
RUN install2.r --error --skipinstalled lubridate
RUN install2.r --error --skipinstalled ivreg
RUN install2.r --error --skipinstalled tsibble
RUN install2.r --error --skipinstalled broom
RUN install2.r --error --skipinstalled moexer

RUN rm -rf /tmp/downloaded_packages

# Copy your local R script to the container directory
COPY MonetaryPolicyShocksEstimation.R /home/r-environment/MonetaryPolicyShocksEstimation.R
COPY BoRMPSchedule.csv /home/r-environment/BoRMPSchedule.csv
COPY BoRSiteParser.R /home/r-environment/BoRSiteParser.R
COPY YieldCurve.csv /home/r-environment/YieldCurve.csv
COPY YieldCurveConstruction.R /home/r-environment/YieldCurveConstruction.R
COPY Server.R /home/r-environment/Server.R
COPY App.R /home/r-environment/App.R



# Set the working directory
WORKDIR /home/r-environment

# Command to run the R script when the container starts
CMD ["R", "-e", "source('Server.R')"]
