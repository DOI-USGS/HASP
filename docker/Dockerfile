FROM code.usgs.gov:5001/water/dockerr:latest

# Necessary R libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  r-cran-dataretrieval \
  r-cran-tidyverse \
  r-cran-ggpp \
  r-cran-scales \
  r-cran-checkmate \
  r-cran-whisker \
  r-cran-xfun \
  r-cran-usethis \
  r-cran-EnvStats \
  r-cran-readxl \
  r-cran-shiny \
  r-cran-shinycssloaders \
  r-cran-shinydashboard \
  r-cran-shinyace \
  r-cran-openxlsx \
  r-cran-oce \
  r-cran-kableextra \
  r-cran-dt \
  r-cran-leaflet \
  r-cran-rkt \
  r-cran-zyp \
  r-cran-plotly \
  r-cran-rlang \
  r-cran-knitr \
  r-cran-waterdata \
  r-cran-rsconnect \
  && rm -rf /var/lib/apt/lists/*



