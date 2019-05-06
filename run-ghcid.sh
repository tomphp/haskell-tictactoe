#!/usr/bin/env bash
ghcid --command "stack ghci haskell-tictactoe:lib haskell-tictactoe:test:haskell-tictactoe-test --ghci-options=-fobject-code" -T=main
