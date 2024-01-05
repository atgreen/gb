FROM registry.access.redhat.com/ubi7/python-36

RUN  pip install gcalcli
COPY ./root /

CMD /usr/bin/gcal-poll
