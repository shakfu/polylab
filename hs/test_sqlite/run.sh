#!/bin/bash
clear
ghc TestDB.hs --make
rm TestDB.hi TestDB.o
./TestDB

