# λtorrent

An attempt at implementing the BitTorrent protocol in Haskell because 
- I am doing a course on Functional Programming.
- I am reasonably sure I am a masochist. 
- Torrents are fun, piracy is not evil.


### Current Status
- Fully functional, monadic **Bencode** parser, written without using any parsing libraries. 
- An implementation of the **Tracker** Protocol, can get a list of peers, and verify hashes.

### WIP
- Implementing the peer wire protocol. For some reason TCP connections take a long time to materialize.
- Magnet link parser because piratebay has moved away from .torrent files. 

### Future Work
- Put downloaded chunks together. 
- Maybe figure out seeding. 

### Trivia
- Why is it called λTorrent? If you are asking this you are clearly not an FP appreciator. 
- What is a monad? A monad is a monoid in the category of endofunctors.
