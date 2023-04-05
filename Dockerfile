FROM rocker/verse

RUN Rscript --no-restore --no-save -e "update.packages(ask = FALSE);"
RUN R -e "install.packages(\"tidyverse\")"
RUN R -e "install.packages(\"lubridate\")"
RUN R -e "install.packages(\"gridExtra\")"
RUN R -e "install.packages(\"markdown\")"
RUN R -e "install.packages(\"gt\")"

RUN bash update-tlmgr-latest.sh

