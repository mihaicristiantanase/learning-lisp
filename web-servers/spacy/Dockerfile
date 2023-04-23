FROM alpine
RUN apk add python3 python3-dev
RUN python3 -m ensurepip
RUN apk add gcc g++
RUN pip3 install -U spacy
RUN pip3 install falcon
RUN python -m spacy download en_core_web_sm
COPY python_spacy_nlp_server/ /python_spacy_nlp_server/
WORKDIR /python_spacy_nlp_server/
RUN python setup.py install
CMD ["/usr/bin/python", "spacynlpserver/server_spacy.py"]