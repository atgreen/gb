FROM registry.access.redhat.com/ubi9/python-311

RUN  pip install gcalcli
COPY ./root /

CMD /usr/bin/gcal-poll
