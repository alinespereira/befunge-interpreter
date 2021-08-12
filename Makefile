start:
	ghcid -c "stack repl"

.PHONY: test
test:
	ghcid -c 'stack ghci --test' --test 'Test.Hspec.hspec' 