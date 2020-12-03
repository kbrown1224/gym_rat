FROM r-base:3.6.3

RUN apt-get update -y
RUN apt-get install -y libpq-dev

RUN mkdir /home/gym_rat
COPY ./ /home/gym_rat

WORKDIR /home/gym_rat

RUN R -e "install.packages('renv')"
RUN R -e "renv::restore()"

EXPOSE 9999

CMD R -e "shiny::runApp(host = '0.0.0.0', port = 9999, launch.browser = FALSE)"
