FROM ghcr.io/jamesdbrock/ihaskell-notebook:master

ENV SRC="/tdf" IH="/opt/IHaskell" IF="./image-files" FP="/usr/local/bin/fix-permissions"

USER root

RUN apt-get update -y -q && apt-get install -y neovim

ADD . $SRC
RUN $FP $SRC
RUN cp $IH/stack.yaml $IH/stack.yaml.orig
RUN cp $IH/ihaskell.cabal $IH/ihaskell.cabal.orig

ADD $IF/opt-ihaskell-stack.yaml $IH/stack.yaml
ADD $IF/ihaskell.cabal $IH/ihaskell.cabal

WORKDIR $IH

USER $NB_UID

# RUN stack build
RUN stack install

WORKDIR /home/$NB_USER
