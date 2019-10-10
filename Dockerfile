FROM haskell:8.6.5
RUN apt update && apt full-upgrade -y && apt install bash-completion cabal-install -y
RUN useradd -ms /bin/bash kiwi
USER kiwi
WORKDIR /home/kiwi/code
RUN cabal update; cabal install QuickCheck