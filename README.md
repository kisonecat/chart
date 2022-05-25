# Charts for plotting (academic) courses

A 'chart' is a node in a decentralized network of gradebooks, and it stores local data for learners and their progress on worksheets.  A navigational chart helps plan geographical courses; these charts helps plan (academic) courses.  In summary, a chart

- provides identity for learners
- handles course enrollment
- stores the course gradebook
- stores "progress" in that gradebook

## Dependencies

We use https://redis.io/ to store data.

## How to run this
 
I use Nix and cabal. 

```
nix-shell
openssl genrsa -out key.pem 4096
cabal build doenetchart
cabal run doenetchart
```

