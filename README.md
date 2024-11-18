# λtorrent

An attempt at implementing the BitTorrent protocol in Haskell because 

### Current Status
- Fully functional, monadic **Bencode** parser, written without using any parsing libraries. 
- A complete implementation of the **Tracker** Protocol, can get a list of peers, and verify hashes.
- A (possibly very inefficient) implementation of a subset of the **Peer Wire** protocol. For some reason TCP connections take a long time to materialize, so we wrote a dead simple echo server to as the proof of concept. It _should_ work with _real_ peers but I haven't verified this. It will work with minimal changes anyway. 

### Installaltion Instructions
- Ensure you have **haskell**, **cabal**, and **stack** installed. 
- Clone the master branch of this repository. 
- In the directory, run 
```bash
cabal build
```
- To use the repl, use ```bash
cabal repl
``` 

This loads all libraries to ghci, can interact with functions. 
- To directly run the tcp demo, open two terminals. In one terminal run


```bash
python3 tcp.py <port>
```
- In the other terminal, run 
```
cabal run
```
and follow the prompts. 

### Future Work
- Make the PWP implementation better. 
- Maybe figure out seeding. (**Update** : Its figured out, it just do not have enough RAM).



