# Copyright (c) 2014 Sina Samavati <sina.samv@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

.PHONY: all app compile shell clean

PROJECT = zeta
OTP_VSN = $(shell erl -noshell -noinput -eval \
		'io:format(lists:sublist(erlang:system_info(otp_release), 3)), \
		halt()')
ERLC_FLAGS = -Werror +debug_info +warn_shadow_vars
EBIN = $(CURDIR)/ebin
PLT_FILE = $(CURDIR)/.$(PROJECT).plt

all: compile app

app: compile ebin/$(PROJECT).app

compile: $(EBIN) $(patsubst src/%.erl, ebin/%.beam, $(wildcard src/*.erl))

ebin/%.beam: src/%.erl
	erlc -v -o ebin -D_$(OTP_VSN) $(ERLC_FLAGS) $<

ebin/%.app: src/%.app.src
	cp $< $@

shell:
	erl -pa ebin

test: clean app
	erlc -v -o test $(ERLC_FLAGS) $(wildcard test/*.erl test/*/*.erl)
	@mkdir -p logs
	ct_run -suite zeta_SUITE \
		-no_auto_compile \
		-noshell \
		-pa $(EBIN) \
		-dir test \
		-logdir logs
	@rm -f test/*.beam

dialyze: $(PLT_FILE)
	@dialyzer +S 4 --src src --plt $(PLT_FILE) --no_native -Werror_handling \
		-Wrace_conditions

$(CURDIR)/%.plt:
	dialyzer +S 4 --build_plt --output_plt $@ --apps erts kernel stdlib

clean:
	rm -rf $(EBIN) erl_crash.dump

distclean: clean
	rm -f $(PLT_FILE)

$(CURDIR)/%:
	mkdir -p $@
