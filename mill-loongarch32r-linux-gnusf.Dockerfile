FROM cuibst/scala-mill:latest  

# this is a Debain 11 image

SHELL ["/bin/bash", "-c"]

# config toolchains
RUN wget https://gitee.com/loongson-edu/la32r-toolchains/releases/download/v0.0.2/loongarch32r-linux-gnusf-2022-05-20-x86.tar.gz
RUN tar -xvf loongarch32r-linux-gnusf-2022-05-20-x86.tar.gz
RUN rm loongarch32r-linux-gnusf-2022-05-20-x86.tar.gz
RUN echo 'export PATH=/root/loongarch32r-linux-gnusf-2022-05-20/bin:$PATH' >> .bashrc

# install `make`
RUN apt-get update
RUN apt-get update && apt-get install -y make gcc g++
RUN rm -rf /var/lib/apt/lists/*

# as mill-loongarch32r-linux-gnusf