FROM ghcr.io/jamesdbrock/ihaskell-notebook:master

USER root

RUN apt-get update -y -q && apt-get install -y neovim
ADD . /tdf
RUN /usr/local/bin/fix-permissions /tdf
RUN cp /opt/IHaskell/stack.yaml /opt/IHaskell/stack.yaml.orig
RUN cp /opt/IHaskell/ihaskell.cabal /opt/IHaskell/ihaskell.cabal.orig
ADD ./docker-files/stack.yaml /opt/IHaskell/stack.yaml
ADD ./docker-files/ihaskell.cabal /opt/IHaskell/ihaskell.cabal

WORKDIR /opt/IHaskell

USER $NB_UID

RUN stack build

WORKDIR /home/jovyan



