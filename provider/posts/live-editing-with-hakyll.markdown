---
title: Live Editing with Hakyll
published: December 20, 2013
excerpt: See changes as you write them
tags: Hakyll, Haskell
---

A recent trend in developer-oriented blogging is to use static site generators, perhaps the most popular being Jekyll written in Ruby. The general workflow when writing posts with static site generators is the classic edit, save, reload.

* toc

A while back, a friend was telling me about his Jekyll setup and in passing mentioned how indispensable some "LiveReload" tool was. I previously did use Jekyll myself, but I hadn't heard of this LiveReload tool. For me, reloading didn't feel like such a hassle, so having a tool that "only" automatically reloaded the page for me didn't feel entirely beneficial.

For this reason, I assumed that this was **not** what the LiveReload tool did, and instead I assumed it somehow replaced the content (i.e. post body) in-place. It turns out I was incorrect in my assumption and ended up implementing what I thought LiveReload did in Hakyll, the static site generator I use written in Haskell.

# Conception

I had to come up with a way to intercept the post body content when it was altered, and send that to the client, who would then simply replace the content div with the received content. Conceptually, it was a pretty straightforward process.

I decided that the communication channel between the Hakyll preview server and the client would be a WebSocket, so I looked for websocket packages on Hackage and [found one][websockets] by the author of Hakyll himself. There are backends for [wai] and [snap], but starting up a wai or snap server seemed too heavy for what I was going to do.

[websockets]: http://hackage.haskell.org/package/websockets
[wai]: http://hackage.haskell.org/package/wai-websockets
[snap]: http://hackage.haskell.org/package/websockets-snap

Having written Hakyll and Pandoc compilers before, I knew that the best place to get the changes to a post would be within the Hakyll compiler pipeline, before templates are applied.

``` haskell
compile $ getResourceBody
  >>= pandocCompiler (storeDirectory conf)
  >>= pushToClient
  >>= loadAndApplyTemplate itemTemplate context
  >>= loadAndApplyTemplate "templates/layout.html" layoutContext
```

Inserting a compiler at this appropriate location would give me access to changes to posts as they were applied, allowing me to then send them to the client via WebSocket.

# Server Architecture

The WebSocket server that comes with the websockets package forks separate threads for every client that connects. I fork the WebSocket server itself within the site binary to avoid interrupting the main Hakyll compilation process. Therefore there needs to be a way for the Hakyll compiler to communicate with the WebSocket client threads, specifically to send each client the changes as they're made to a post.

* main thread
    * hakyll
    * WebSocket server
        * client thread 1
        * client thread 2
        * client thread N

## State

First, however, I had to think about how the WebSocket server would communicate with the clients. I decided that the most straightforward thing to do would be to communicate on a per-route basis. A route in this case refers to a post, such as `posts/live-editing-with-hakyll.markdown.`{.path} The clients use that path to establish the connection, which tells the WebSocket server that they're interested in data about that particular post. This way, the client doesn't get updates about posts it doesn't care about.

An alternative to this would've perhaps been to use a single communication channel, where the server would send data about every post that was changed, and clients decided which applied to them. While this seems simpler, it has the consequence that _every_ post's data is sent to the client, even posts no one is actually paying attention to. This is inefficient, and to avoid this inefficiency would again require some form of "interest-registration" which is implicit in the aforementioned method.

For this reason, the server needs a bit of state to keep track of which connections care about which routes, best represented by a `Map` of routes to connections, something like:

``` haskell
Map.Map Route Connections
```

## Communication

Haskell provides a variety of concurrency primitives, such as [MVars] and [Channels]. MVars are mutable locations in memory and Channels are simply FIFO channels. It made sense to use a channel between the Hakyll compiler and the WebSocket client threads, so that WebSocket client threads would subscribe/listen to the channel and the Hakyll compiler would publish/write to the channel with the new post data.

[MVars]: http://hackage.haskell.org/package/base/docs/Control-Concurrent-MVar.html
[Channels]: http://hackage.haskell.org/package/base/docs/Control-Concurrent-Chan.html

However, we're actually going to be using the [stm] package's variants of these concurrency primitives: [TVars] and [TChans]. The [Software Transactional Memory] package allows us to perform transactional, atomic operations to avoid race conditions. We're going to use a `TVar` for the server state, which will allow the Hakyll and WebSocket server to perform transactional read/write operations on the server state without stepping on each others' toes (i.e. race conditions).

I chose a `TVar` and not a `TMVar` because we won't have a concept of an "empty state," which is what the `TMVar` variant allows. The state will always contain something, even if that's just an empty `Map`. So let's wrap our state up in a `TVar`:

[stm]: http://hackage.haskell.org/package/stm
[TVars]: http://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM-TVar.html
[TChans]: http://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM-TChan.html
[Software Transactional Memory]: http://en.wikipedia.org/wiki/Software_transactional_memory

``` haskell
type State = TVar (Map.Map Route Connections)
```

There's a slight problem with our idea of using channels, however. Reading a value from a channel "consumes" that value, i.e. it's no longer available for reading from that channel. This complicates things because it means that I couldn't easily have a single channel to write every post's changes to because that would mean that only the first client thread to read the data would receive the data.

Fortunately, the channel type supports a duplication operation. If you have channel `A` and you duplicate it, yielding channel `B`, writing to any one of those channels makes the data available to read from both channels:

> Duplicate a Chan: the duplicate channel begins empty, but data written to either channel from then on will be available from both. Hence this creates a kind of broadcast channel, where data written by anyone is seen by everyone else.

Remember that our server state consists of a `Map` of routes to connections. Instead, we could make this a `Map` of routes to broadcast channels. When the Hakyll compiler updates a post, it'll see if there's a broadcast channel associated with the post's route and if so, it'll pipe the data through that broadcast channel. On the WebSocket client thread side, they will duplicate the broadcast channel for that route they're interested in, or if it doesn't already exist, create it first.

One final point is that we would like to avoid writing to a channel if no one is listening,  to avoid unnecessary work. A simple way to achieve this is through simple reference counting. When a new client listens in on the broadcast channel, we increment the reference count, and accordingly decrement it when they disconnect, removing the channel from the Map altogether if no one is listening anymore.

To be precise, we will be creating the broadcast channel with `newBroadcastTChan`, which is recommended when creating a broadcast channel. This is because creating a broadcast channel with `newTChan` has the consequence that it's treated as any other channel that can be read from, so data begins to pile up as it's written since it's never read from, only the duplicate channels are read from. Creating one with `newBroadcastTChan` closes the read stream which allows the garbage collector to dispose of items once they're read from the duplicate channels.

# Implementation

Now that we've planned stuff out we can get to the implementation. As a summary, this is what the WebSocket server will be doing:

1. client opens post which initiates connection to WebSocket server for that post
2. WebSocket server forks off a thread to handle that client
    1. client checks server state to see if broadcast channel already exists for the post
        * exists
            1. duplicate the broadcast channel
            2. increment reference count
        * doesn't exist
            1. create a broadcast channel for this post
            2. put it in server state with initial reference count of 1
            3. duplicate it
    2. listen in on duplicate channel for changes to the post
    3. when data is received from the channel, pipe it to WebSocket client
    4. repeat steps 2-3 forever

And here's what the Hakyll compiler will be doing:

1. retrieves item body
2. checks if there's a broadcast channel available for this item
    * yes: pipe the item body through the channel
    * no: do nothing

First we have our server state, which consists of a map from routes to pairs of channels and their reference counts. Once again, we'll store this state in a `TVar` so that it can be read and written to from Hakyll and the WebSocket server in a transactional manner.

``` haskell
type Channels = TVar (Map.Map String (TChan String, Integer))
```

## WebSocket Server

The WebSocket server is pretty straightforward. We'll print a message out and listen in on port 9160, specifying a client connection handler `wsHandler` that needs access to the server state:

``` haskell
wsServer :: Channels -> IO ()
wsServer channels = do
  putStrLn "WebSocket Server Listening on http://0.0.0.0:9160/"
  WS.runServer "0.0.0.0" 9160 $ wsHandler channels
```

The client handler starts out in a straightforward manner. We begin by getting the request data, from which we yield the path that the client connected to. This path, after all, signifies the post the client is interested in. Then we proceed to accept the client connection:

``` haskell
wsHandler :: Channels -> WS.ServerApp
wsHandler channels pending = do
  let request = WS.pendingRequest pending
      path    = tail . BC.unpack $ WS.requestPath request

  conn <- WS.acceptRequest pending
```

Next we need to get the channel we'll be listening in on. Notice that this is performed `atomically` within the `STM` monad, since we don't want to have race conditions between the time we read the map and when we update the server state with either an incremented reference count or a new entry.

We begin by retrieving the server state from the `TVar`. We then perform a lookup in the Map to determine if a channel already exists for the given path. If a channel **doesn't** exist, we create a new broadcast channel using `newBroadcastTChan` and insert it into the server state Map with an initial reference count of 1. Finally we duplicate this broadcast channel using `dupTChan`, which will be our result. If a broadcast channel already does exist, then we simply increment the reference count and duplicate it.

``` haskell
  chan <- liftIO $ atomically $ do
    chans <- readTVar channels

    case Map.lookup path chans of
      Just (ch, refcount) -> do
        modifyTVar' channels $ Map.insert path (ch, refcount + 1)
        dupTChan ch
      Nothing -> do
        ch <- newBroadcastTChan
        modifyTVar' channels $ Map.insert path (ch, 1)
        dupTChan ch
```

Now that we have the correct channel to listen from, we can forever perform the same loop:

1. read from the channel; this blocks until there's something to read
2. pipe the data to the WebSocket

``` haskell
  handle catchDisconnect . forever . liftIO $ do
    atomically (readTChan chan) >>= WS.sendTextData conn . T.pack
```

Finally we need to gracefully handle the case where the client leaves by either decrementing the reference count or outright removing the channel from the Map. This is also an atomic operation since we're performing a read followed by a write:

``` haskell
  atomically $ do
    chans <- readTVar channels
    case Map.lookup path chans of
      Just (ch, refcount) -> do
        if (refcount - 1) == 0
          then modifyTVar' channels $ Map.delete path
          else modifyTVar' channels $ Map.insert path (ch, refcount - 1)
      Nothing -> return ()
```

## Hakyll Compiler

Remember that we also need a Hakyll compiler to insert into the Hakyll compiler pipeline. This compiler will read the server state `Map` and determine if if there's a channel to send the item body to, and if there isn't, does nothing.

We will need two pieces of data relevant to the `Item` being compiled: the path to the file responsible for this `Item` as well as the `Item`'s body. The path is what we'll use as the key into the Map, and the body is what we'll pipe through the channel:

``` haskell
webSocketPipe :: Channels -> Item String -> Compiler (Item String)
webSocketPipe channels item =
  unsafeCompiler $ do
    let path = toFilePath . itemIdentifier $ item
        body = itemBody item
```

We fork off another thread to atomically retrieve the server state and determine if there's a channel associated with the path, and if so, pipes the body through the channel:

``` haskell
    void . forkIO $ atomically $ do
      chans <- readTVar channels

      case Map.lookup path chans of
        Just (ch, _) -> writeTChan ch body
        Nothing -> return ()
```

The final and very important thing to do is to return the item as it was passed to us. This in effect makes this Hakyll compiler transparent, just "observing" the data that's passing through it.

``` haskell
    return item
```

## Main Thread

The main thread should begin by initializing the server state:

``` haskell
  channels <- atomically $ newTVar Map.empty
```

You'll need a way to determine whether the WebSocket server should run. In my case I have a `previewMode` variable that's only true when the Hakyll action is either watch or preview; everything else defaults to deploy-mode.

``` haskell
  let previewMode = action == "watch" || action == "preview"
  when previewMode $ void . forkIO $ wsServer channels
```

Finally, don't forget to insert the Hakyll compiler into the pipeline, passing it the server state:

``` haskell
    compile $ getResourceBody
      >>= pandocCompiler (storeDirectory conf)
      >>= webSocketPipe channels
      >>= loadAndApplyTemplate itemTemplate context
      >>= loadAndApplyTemplate "templates/layout.html" layoutContext
```

## Client Side

Now we need to wire stuff up from the client side. First though there's one last thing we need to do in the back-end. We need to create a Hakyll `Context` that inserts the client-side JavaScript only if we're in preview mode, otherwise when you deploy your site every visitor will be attempting to connect to the WebSocket server.

In my setup I have a `postCtx` that specifies the `Context` to use for posts. I've changed it to be a function that takes as argument a `Bool` specifying whether or not the site is in preview mode. This indicator is further passed on to a function called `pushJS` that will embed the JavaScript if it's `True`:

``` haskell
postCtx :: Bool -> Context String
postCtx preview = mconcat
  [ pushJS preview "pushJS"
  -- ...
  , defaultCtx ]
```

Before we get to `pushJS`, consider that we might want to disable this feature on a per-post basis. Personally I'd like this functionality to be on by default, but there are some posts I have such as [this one](/notes/machine-learning) that are ridiculously long and take a very long time to load, so I'd like to be able to set a metadata `push: off` option in that particular post to disable it. Let's define a function that gets the metadata value and assumes its `True` unless it's explicitly set to `false` or `off`:

``` haskell
pushOn :: (MonadMetadata m) => Item a -> m Bool
pushOn item = do
  pushMeta <- getMetadataField (itemIdentifier item) "push"
  return $ case pushMeta of
             Just "false" -> False
             Just "off" -> False
             _ -> True
```

Now we can get to the `pushJS` function. We only generate the JavaScript code if the site is in preview mode and the option isn't disabled for this particular `Item`. The way this will work is that it'll load the contents of the file `templates/push-js.html` into the `key` tag, which is `"pushJS"` in my case as defined above. So in my layout template I'll have `$pushJS$`, which will be replaced by the contents of `push-js.html` or it'll be an empty string if the requirements for the feature aren't met. One last thing is that we pass the path of the file responsible for the `Item` into the `push-js.html` template as the `$path$` tag so that the WebSocket knows what path to connect to:

``` haskell
pushJS :: Bool -> String -> Context String
pushJS preview key = field key $ \item -> do
  push <- pushOn item
  if preview && push
    then do
      path <- fmap toFilePath getUnderlying
      tmpl <- loadBody "templates/push-js.html"
      itm <- makeItem "" :: Compiler (Item String)
      gend <- applyTemplate tmpl (constField "path" path) itm
      return $ itemBody gend
    else return ""
```

The contents of the file `push-js.html` are pretty straightforward. We use the `$path$` that we were passed by `pushJS` to connect to the WebSocket server. Then we define an `onmessage` handler. This handler does a couple of things. First it finds the element that contains my post body. Once it has the element, it replaces the contents of this element with the data received through the WebSocket.

We then have to perform some house cleaning, essentially re-running JavaScript functionality that ran on DOM load, such as creating links out of the headers in the post. I wrapped this stuff up in a global `refresh` function. This allows me to simply call it again in this handler. The last thing I do in this handler is re-run MathJax $\LaTeX$ typesetting on the post body element, since it originally ran on DOM load as well.

``` html
<!-- preview push -->
<script async="true" type="text/javascript">
  jQuery(function (){
    var ws = new WebSocket('ws://localhost:9160/$path$');
    ws.onmessage = function (e) {
      var content = jQuery('article .entry-content');
      content.html(e.data);

      window.refresh();

      MathJax.Hub.Queue(["Typeset", MathJax.Hub, content[0]]);
    };
  });
</script>
```

# Conclusion

So I added this functionality to Hakyll that I thought others had through LiveReload. Of course, I showed it to the friend that told me about LiveReload to begin with and he was amazed, as was I when he told me that all LiveReload did was refresh the page for you.

Of course, the advantage of LiveReload over this is that it handles any asset, such as style sheets, and reloads the entire page so that you can see those changes, rather than just the post body. But like I mentioned in the beginning of this post, I don't feel like I need that functionality in particular. Perhaps I'll implement it later on as well anyways though, for convenience. I have a feeling it'll be more straightforward than this.

