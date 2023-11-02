# 🚧 Work in Progress
![Project Status](https://img.shields.io/badge/status-Work%20in%20Progress-yellow)

This repository is currently in experimental phase, and it may contain bugs. The public API is subject to significant changes.

# 🎯 Project Objectives

Here I try to migrate my other UI library [htmlt](https://github.com/lagunoff/htmlt) into GHC WebAssembly Backend. Alongside this, I also wanted to try some experimental changes in underlying implementation, while keep API relatively the same. If this project is successfull I hope to gain following benefits:
  - Faster compilation times
  - More compact executables
  - Fast hot-reloading capabilities (when the changes appear in the browser under a second after source file is saved)
  - Faster runtime speeds (it does seem to be faster, but I wasn't really counting on that, WebAssembly has perfomance trade-offs for interactive applications)
  - All that with fewer dependencies as possible (compiling to WebAssembly only requires base packages, while the Native GHC will need a few WAI packages to run a dev server)

# 🔄 How It Interacts With JavaScript

The Haskell program initially compiles into so-called WASI Reactor module. In contrast to a regular program that terminates when the main function finished, the Reactor exports specific functions through FFI. These functions can be called from the JavaScript side and the program maintains it's state between the calls. Haskell and JavaScript talk to each other through shared memory with serialized commands using `Data.Binary` format (it's a questionable choice but at least it doesn't introduce new dependencies). 

# 🔥 Hot-Reloading with ghcid

The choice of binary commands to interface with JavaScript has following drawbacks and benefits:
1) Makes harder to FFI with JavaScript (you have to construct Lambda-style AST instead of writing more natural JavaScript)
2) It makes possible to run client apps with Native GHC executables 
3) It makes much easier to implement hot-reloading because the commands can be passed through a wire, other stuff works exacly the same way

[![Hot-Reloading Demo](https://lagunoff.github.io/htmlt-wasm/screen-recorder-thu-oct-19-2023-15-37-20.jpg)](https://lagunoff.github.io/htmlt-wasm/screen-recorder-thu-oct-19-2023-15-37-20.webm "Hot-Reloading Demo")

# 🌟 Benefits compare to Reflex+GHCJS+JSaddle

The key benefit of htmlt-wasm is that is has much fewer dependencies and much fewer lines in actual implementation, providing similar value. Also the code is much more understandable. This is my personal opinion of course and other people will have different perspectives, I admit it's far from being ideal at the time of writing, but I'll try my best to cleanup the code abd provide documentation as the codebase matures. Also if you use `JSaddle` with `GHCJS` you might noticed an annoying fact that you have to write two versions of FFI for `GHCJS` and `JSaddle` that can potentially diverge and can lead to production bugs. In contrast htmlt-wasm you don't have this distinction it works similar way to `JSaddle` either throught websockets (on a devserver) or throught WASM shared memory (in production) using the same protocol. This protocol also optimized to minimize round-trips resulting in faster hot-reloading experience compared to `JSaddle`.


# 🐞 WebAssembly Backend Bug

Unfortunately I encountered a bug in WebAssembly Backend — reactor module crashes after several GC sweeps. To counteract that I tweak RTS options (greadymem examples) that prolong application lifetime in exchange for high memory consumption. Hopefully the bug will be addressed in near future

# 🛠️How to build examples

First thing you'll need is [nix](https://nixos.org/download.html) package manager. The last thing you need — is to have any server for hosting static files for WebAssembly examples

```sh
# Clone this repository with submodules
git clone --recurse-submodules https://github.com/lagunoff/htmlt-wasm.git
cd htmlt-wasm
```
### Build For WebAssemly Backend
```sh
# Build examples with cabal inside ghc-meta-wasm shell
nix shell ./ghc-wasm-meta -c wasm32-wasi-cabal build todomvc voting --offline 
# Now enter dist-newstyle and run a static server of your choice
cd dist-newstyle
php -S 0.0.0.0:8001
# Now you can open http://localhost:8001/todomvc.html in your browser
```
### Run hot-reloading server with Native GHC
```sh
# Start dev-server for todomvc example
nix-shell --run 'ghcid -c "cabal repl todomvc --offline" --test main'
# Now you can open http://localhost:8002/ in your browser
```
### Update JavaScript Runtime
You'll need nodejs and yarn package manager
```sh
# Install dependencies
yarn install
# Update dist-newstyle/index.bundle.js
yarn run webpack --mode production
# Don't forget to update the runtime for DevServer in src/HtmlT/Wasm/DevServer.hs
```

# 💡 Examples built with WebAssembly

<table>
  <tbody>
    <tr>
      <td>Simple Voting</td>
      <td><a href=./examples/voting/ target=_blank>source</a></td>
      <td>
        <a href=https://lagunoff.github.io/htmlt-wasm/examples/voting.html target=_blank>open</a>
      </td>
      <td>
        <a href=https://lagunoff.github.io/htmlt-wasm/examples/voting-greedymem.html target=_blank>greadymem hack</a>
      </td>
    </tr>
    <tr>
      <td>TodoMVC</td>
      <td><a href=./examples/todomvc/ target=_blank>source</a></td>
      <td>
        <a href=https://lagunoff.github.io/htmlt-wasm/examples/todomvc.html target=_blank>open</a>
      </td>
      <td>
        <a href=https://lagunoff.github.io/htmlt-wasm/examples/todomvc-greedymem.html target=_blank>greadymem hack</a>
      </td>
    </tr>
  </tbody>
</table>

# ✅ TODOs

 - [ ] Gather data for the WebAssembly Backend bug report, make an isolated counterexample
 - [x] Add Benchmarks
 - [ ] <s>Decouple RJS's built-in reactive capabilities into a separate ReactiveT transformer</s>. Tried it, didn't work, polymorphic ReactiveT quickly became overly complex, so I'm keeping it all concreete and straightforward inside a single RJS monad
 - [ ] Improve messaging protocol (make a version that is readable JSON and another compact and fast binary version)
 - [ ] Review, cleanup the code and write documentation
 - [ ] Try various protocol optimizations (use bounded buffer instead of using malloc/free for each message)
 - [ ] Explore possible optimizations in the JavaScript implementation of the protocol (use hand written encoders/decoders instead of reflection on datatype description)
 - [ ] Use `Data.Text` instead of `HtmlT.Protocol.Utf8`?


