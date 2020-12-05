# parent image
FROM resin/raspberrypi3-debian:latest

ENTRYPOINT []

# enable systemd
ENV INITSYSTEM on

# update sources
RUN echo "deb http://mirrordirector.raspbian.org/raspbian/ buster main" > /etc/apt/sources.list


# install R
RUN apt-get update && apt-get install -y \ 
  r-base \ 
  r-base-core \
  r-base-dev \
  libpq-dev

RUN mkdir /home/gym_rat
COPY ./ /home/gym_rat

WORKDIR /home/gym_rat

RUN R -e "install.packages('renv')"
RUN R -e "renv::restore()"

EXPOSE 9999

CMD R -e "shiny::runApp(host = '0.0.0.0', port = 9999, launch.browser = FALSE)"
