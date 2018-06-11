# editaxiom
Experimental Web IDE for Haskell using transient


Now you can play with it with

```
docker run -p 8000:8000 agocorona/transient
```

This will start the web server at this port.

Then point the borwser to  localhost:8000

This is the message when the image is executed:

```
$ docker run -p 8000:8000 agocorona/transient
Unable to find image 'agocorona/transient:latest' locally
latest: Pulling from agocorona/transient
Digest: sha256:a4195a1b7d4307fb212a3f9df6b504f202cc387938968466eaf721a0290e013f
Status: Downloaded newer image for agocorona/transient:latest

WELCOME to Transient

https://github.com/transient-haskell/transient

docker run -p 8000:8000 agocorona/transient:22-05-2018   -- executes an IDE with examples at port 8000
                                                         -- the 'examples' folder contains some examples to build/execute


docker run -p 8000:8000 agocorona/transient:22-05-2018 bash    -- executes bash, you have ghc and ghcjs, cabal stack etc.



Enter  options  to: show all options
Enter  ps       to: show threads
Enter  log      to: inspect the log of a thread
Enter  end      to: exit
Enter  start    to: re/start node
Executing: "start/localhost/8000"
"start"

option: start
hostname of this node. (Must be reachable)? "localhost"
port to listen? 8000
```

