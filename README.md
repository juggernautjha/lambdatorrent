# λtorrent

An attempt at implementing the BitTorrent protocol in Haskell because 
- I am doing a course on Functional Programming.
- I am reasonably sure I am a masochist.
- Torrents are fun, piracy is not evil.

### Current Status
- Fully functional, monadic **Bencode** parser, written without using any parsing libraries. 
- An implementation of the **Tracker** Protocol, can get a list of peers, and verify hashes.
- A (possibly very inefficient) implementation of the **Peer Wire** protocol. For some reason TCP connections take a long time to materialize, so we wrote a dead simple echo server to as the proof of concept. It _should_ work with _real_ peers but I haven't verified this. It will work with minimal changes anyway. 


### WIP

### Future Work
- Make the PWP implementation better. 
- Put downloaded chunks together. 
- Maybe figure out seeding. (**Update** : Its figured out).
- Magnet link parser because piratebay has moved away from .torrent files. Should be a drop in replacement.  (**Update** : Nope. Requires an understanding of DHTs, and I am retarded).


### Trivia
- Why is it called λTorrent? If you are asking this you are clearly not an FP appreciator. 
- What is a monad? A monad is a monoid in the category of endofunctors.
- Why do you hate GHCI? The circumstances we met in were not conducive to a healthy relationship. For the lack of a better term, GHCI is a memory hog and I managed to fry one of my two precious sticks of RAM some time back. 
- If you eat enough Saridons, you save money on Redbulls. 
