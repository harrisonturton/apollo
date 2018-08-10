# 🌙 Apollo

For Haskell beginners, the barrier to writing "real-world" programs is high. We have to push through alien syntax, a strict compiler before finally tackling the mountain that is `Control.Monad`.

Current frameworks are confusing for beginners, with nested monads and arcane symbols — what the heck does `<|>` mean?

**Apollo is for people who've just finished LYAH.** It features a small, readable (and well commented!) codebase. Only the simple `Reader` and `State` monads are exposed to the user.

### Features

- Simple API
- Easy-to-understand types
- Small codebase
- Regex-matching routes



## Installation

```sh
> git clone https://github.com/harrisonturton/apollo.git
```



## Usage

The canonical "Hello World" server that runs at `localhost:3000`:

```haskell
import Apollo

main = apollo 3000 $ do
  get "/" $ do
    return $ status200 "Hello World!"
```



An echo server that runs at any subdomain `localhost` subdomain — e.g. `localhost:3000/cat`.

```haskell
import Apollo

main = apollo 3000 $ do
  get "/[a-zA-Z]" $ do
    request <- get
    return $ (status200 . body) request
```

