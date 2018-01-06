{ mkDerivation, base, bytestring, concurrency, containers
, criterion, data-memocombinators, deepseq, dejafu, doctest
, exceptions, hashable, MemoTrie, microlens, mtl, QuickCheck
, spiros, StateVar, stdenv, stm, tasty, tasty-quickcheck, text
, transformers, uglymemo, unordered-containers
}:
mkDerivation {
  pname = "memo";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring concurrency containers data-memocombinators deepseq
    dejafu exceptions hashable MemoTrie microlens mtl spiros StateVar
    stm text transformers uglymemo unordered-containers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest QuickCheck tasty tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base criterion deepseq ];
  homepage = "http://github.com/sboosali/memo#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
