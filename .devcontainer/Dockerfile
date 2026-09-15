FROM rocker/r-ver:4.4

# Install system dependencies
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    git \
    openssh-client \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libicu-dev \
    pandoc \
    qpdf \
    ghostscript && \
    rm -rf /var/lib/apt/lists/*

# Configure R to use RSPM for pre-compiled binaries (MUCH faster!)
RUN echo 'options(repos = c(RSPM = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))' >> /usr/local/lib/R/etc/Rprofile.site

# Install R development packages using pre-compiled binaries
RUN Rscript -e "install.packages(c('devtools', 'roxygen2', 'testthat', 'usethis', 'styler', 'lintr', 'covr', 'pkgdown', 'remotes', 'languageserver'))"

# Copy DESCRIPTION file to install package dependencies
COPY DESCRIPTION /tmp/DESCRIPTION

# Install package dependencies during image build
RUN Rscript -e 'remotes::install_deps("/tmp", dependencies = TRUE, upgrade = "never")' && \
    rm /tmp/DESCRIPTION

# Install Claude CLI tool
RUN curl -fsSL https://raw.githubusercontent.com/anthropics/claude-cli/main/install.sh | sh

# Set working directory
WORKDIR /workspace
