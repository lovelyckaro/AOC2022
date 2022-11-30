# 🎄 My solutions for Advent of Code 2022 🎄

Run a day using `cabal run day[number]`, e.g. `cabal run day3`.

In .env put one line with year, and one with a session cookie, see .env.example for example.

To fetch a days description and your input use:
```bash
cabal run fetch [day]
``` 

Days should save solutions in `./answer/day[number]-part[number]`
To actually submit your solutions use
```bash
cabal run submit [day] [part]
```

To build my solutions you're going to have a decently recent version of GHC,
since I use GHC2021. I've built them all using GHC 9.2.4.
