FROM ubuntu

RUN apt-get update && apt-get install -y apt-transport-https

RUN apt-get install -y git

RUN apt-get install -y make

RUN apt-get install -y gcc

RUN apt-get install -y flex

RUN apt-get install -y bison

RUN apt-get install -y libssl-dev

RUN apt-get install -y bc

RUN apt-get install -y libelf-dev

COPY docker-checkout-make.sh /docker-checkout-make.sh

RUN chmod +x /docker-checkout-make.sh

ENTRYPOINT /docker-checkout-make.sh