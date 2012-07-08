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
	erl -pa ebin deps/*/ebin -s nakaz -nakaz priv/test.yaml

# Dialyzer

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

plt-add-deps:
	@$(DIALYZER) --add_to_plt --plt .$(PROJECT).plt \
		--output_plt .$(PROJECT).plt -r deps/

plt-remove-deps:
	@$(DIALYZER) --remove_from_plt --plt .$(PROJECT).plt \
		--output_plt .$(PROJECT).plt -r deps/

plt-readd-deps: plt-remove-deps plt-add-deps

dialyze: app-nodeps
	@$(DIALYZER) --plt .$(PROJECT).plt -r ebin/ \
		-Werror_handling -Wrace_conditions -Wunmatched_returns -Wunderspecs

.PHONY: deps build-plt plt-add-deps plt-remove-deps plt-readd-deps dialyze
