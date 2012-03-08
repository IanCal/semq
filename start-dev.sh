#!/bin/sh
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname semq_dev \
    -s semq \
    -s reloader +K true \
    -detached
