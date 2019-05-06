FROM haskell:8.6.3

WORKDIR /opt/fission-ipfs-api

######################
# Cache common files #
######################

COPY ipfs-api.cabal /opt/fission-ipfs-api
COPY stack.yaml     /opt/fission-ipfs-api

##########################
# Install deps and cache #
##########################

RUN stack update
RUN stack install --only-dependencies -j4
COPY . /opt/fission-ipfs-api

RUN stack install

######################
# Install & run IPFS #
######################

RUN apt-get update

RUN apt-get install apt-utils -y
RUN apt-get install wget golang-go -y

RUN wget https://dist.ipfs.io/go-ipfs/v0.4.10/go-ipfs_v0.4.10_linux-386.tar.gz
RUN tar xvfz go-ipfs_v0.4.10_linux-386.tar.gz
RUN mv go-ipfs/ipfs /usr/local/bin/ipfs
RUN ipfs init
RUN ipfs daemon &

################
# Enter Webapp #
################

CMD ["/usr/local/bin/stack","exec","server"]
