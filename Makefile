REBAR := rebar3

.PHONY: all
all: compile

.PHONY: compile
compile:
	$(REBAR) compile

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build erl_crash.dump rebar3.crashdump rebar.lock

.PHONY: xref
xref:
	$(REBAR) xref

.PHONY: eunit
eunit: compile
	$(REBAR) eunit verbose=truen

.PHONY: ct
ct: compile
	$(REBAR) as test ct -v

.PHONY: cover
cover:
	$(REBAR) cover

.PHONY: dialyzer
dialyzer:
	$(REBAR) dialyzer

.PHONY: fmt
fmt:
	$(REBAR) fmt --verbose -w

.PHONY: fmt-check
fmt-check:
	$(REBAR) fmt --verbose --check
