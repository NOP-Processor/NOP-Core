FROM vivado:2019.2

COPY ./linux_run.tar /
RUN cd / && sudo tar -xf /linux_run.tar
RUN sudo rm /linux_run.tar
RUN sudo chmod -R 777 /linux_run

# as system_test_scratch