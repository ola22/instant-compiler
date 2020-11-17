# instant-compiler
Pierwszy projekt zaliczneiowy z MRJP - kompilatory języka Instant.

## Budowanie
Do budowania projektu użyłam stacka, stworzoneho na podstawie pliku cabala.
Polecenie `make` buduje projekt (wywołuje komendę `stack build`).
Polecenie `make clean` usuwa poleceniem `stack clean --full`
Do budowania projektu i kompilacji wykorzystuję:
* cabal wersja 3.2.0.0
* ghc wersja 8.8.3
* stack wersja 2.5.1

## Uruchamianie programów
W korzeniu projektu znajdują się skrypty wykonywalne `insc_llvm` oraz `insc_jvm`.
Ponieważ miałam problemy z wykopiowaniem plików binarnych z katalogów, utworzonych przez stacka, są to skrypty bashowe, które wywołują komendę `stack exec insc_llvm <given_file_path>` oraz `stack exec insc_jvm <given_file_path>`, uruchamiające binarki kompilatorów.
Po uprzednim zbudowaniu projektu polecenim `make` zgodnie z treścią zadania uruchamia się poleceniem `./insc_llvm <file>` oraz `./insc_jvm <file>`.

## Parser
Z powodu różnic w wersjach i wygenerowanych kodach załączam wygenerowane już przez bnfc pliki parsera.
Wersje użytych w tym celu narzędzi
* bnfc wersja 2.8.1
* alex wersja 3.2.3
* happy wersja 1.19.8

## Użyte zewnętrzne biblioteki + wersje (haskellowe)
array, containers, process, mtl, filepath

# Struktura katalogów i plików projektu

src/ zwiera pliki źródłowe projektu
    Instant/ zawiera opis gramatyki języka instant w formacie BNFC,
           a także automatycznie wygenerowane przez niego `pliki parsera`
    ErrorCheck/ zawiera plik `ErrorChecker.hs` sprawdzający poprawność 
          kompilowanego przez insc_jvm i insc_llvm pliku wejściowego
    `insc_llvm.hs` - plik główny kompilatora llvm
    `insc_jvm.hs` - plik główny kompilatora jvm
lib/ zawiera biblioteki
    `jasmin.jar` - do kompilacji plików jvm
    `runtime.bc` - biblioteka do llvm

`insc_llvm` - skrypt uruchamijący kompilator llvm
`insc_jvm` - skrypt uruchamiający kompilator jvm

`InstantCompiler.cabal` - plik konfiguracyjny cabala
`stack.yaml` - plik konfiguracyjny stacka
`Makefile`
`Readme.md`


Aleksandra Falkiewicz 394182
