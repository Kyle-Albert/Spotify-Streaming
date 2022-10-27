FROM rocker/verse

RUN R -e "install.packages(\"tidyverse\")"
RUN R -e "install.packages(\"lubridate\")"
RUN R -e "install.packages(\"gridExtra\")"

