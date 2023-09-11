# `.devcontainer` Configuration for R Projects

This folder contains the configuration files for setting up a development environment for R projects in a Docker container.

## Files Included

1. `Dockerfile`: This file specifies the Docker image to be used and the additional system dependencies that are installed.
  
2. `r_requirements.txt`: This file lists the R packages that are to be installed when the Docker container is built.

3. `devcontainer.json`: This file is the main configuration file that controls how the dev container should behave.

## Components

### Dockerfile

The Dockerfile is based on the `rocker/r-ver` image from the Rocker Project. Additional system dependencies like `libxml2-dev`, `libcurl4-openssl-dev`, and TeX Live packages are installed.

### r_requirements.txt

This file contains a list of R packages that are installed automatically when the Docker container is built. You can specify the package names one per line in this file.

### devcontainer.json

This file specifies that the `Dockerfile` should be used to build the container, and also installs the Docker extension in the VS Code instance running inside the container.

