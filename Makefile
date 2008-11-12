ERLC=erlc
ERLCFLAGS=
CC=gcc
CFLAGS=-g -O2 -Wall
LD=gcc

all: tidy iconv_erl.so \
	$(patsubst %.erl, %.beam, \
	$(wildcard *.erl))

iconv_erl.so: iconv_erl.c
	$(CC) $(CFLAGS) $< -o $@ \
	-fpic -shared \
	-I/usr/lib/erlang/lib/erl_interface-3.5.7/include \
	-I/usr/lib/erlang/usr/include \
	-L/usr/lib/erlang/lib/erl_interface-3.5.7/lib \
	-lerl_interface -lei

tidy: tidy.c
	$(CC) $(CFLAGS) -c $< -o $@ \
	-I/usr/include/tidy \
	-ltidy

%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) $<
