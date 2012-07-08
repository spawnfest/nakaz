DIALYZER = dialyzer
REBAR = rebar

PROJECT = nakaz

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

app-nodeps:
	@$(REBAR) compile skip_deps=true

run: app-nodeps
	erl -pa deps/*/ebin ebin -s example_app -nakaz priv/conf.yaml

.PHONY: deps app-nodeps run clean app
