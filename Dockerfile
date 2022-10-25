FROM rocker/verse

RUN R -e "install.packages(\"tidyverse\")"

