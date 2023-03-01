FROM openjdk:11

RUN apt-get update -y
RUN curl -fL "https://github.com/coursier/launchers/raw/master/cs-$(uname -m)-pc-linux.gz" | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes --apps cs && \
    ./cs install scala:2.13.10 scalac:2.13.10 sbt:1.8.2 && \
    echo PATH=$PATH:/root/.local/share/coursier/bin
RUN apt-get install -y qemu && \ 
    apt-get install -y qemu-user && \
    apt-get install -y gcc-arm-linux-gnueabi