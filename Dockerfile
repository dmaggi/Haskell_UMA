# Add this to ~/bashrc: "alias haskell="docker-compose -f PATH_FILE/docker-compose.yml up -d; docker attach haskell_UMA""
FROM  haskell:8.4.3
RUN apt update && apt full-upgrade -y && apt install bash-completion cabal-install -y
RUN useradd -ms /bin/bash kiwi
USER kiwi
WORKDIR /home/kiwi/code
RUN cabal update; cabal install QuickCheck