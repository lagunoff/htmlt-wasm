# 🚧 Work in Progress
![Project Status](https://img.shields.io/badge/status-Work%20in%20Progress-yellow)

This repository is currently in experimental phase, and it may contain bugs. The public API is subject to significant changes.

# 🎯 Project Objectives

Here I try to migrate my other UI library [htmlt](https://github.com/lagunoff/htmlt) into GHC WebAssembly Backend. Alongside this, I also wanted to try some experimental changes in underlying implementation, while keep API relatively unchanged. If this project is successfull I hope to gain following benefits:
  - Faster compilation times
  - More compact executables
  - Fast hot-reloading capabilities (when you see the chages in browser in less than a second after source file is saved)
  - Faster runtime speeds (it does seem to be faster, but I wasn't really counting on that, WebAssembly has perfomance trade-offs for interactive applications)
  - All that with fewer dependencies as possible (compiling to WebAssembly only requires base packages, while the Native GHC will need a few WAI packages to run a dev server)

# 🔄 How It Interacts With JavaScript

The Haskell program initially compiles into so-called WASI Reactor module. In contrast to a regular program that terminates when the main function finished, the Reactor exports specific functions through FFI. These functions can be called from the JavaScript side and the program maintains it's state between the calls. Haskell and JavaScript talk to each other through shared memory with serialized commands using `Data.Binary` format (it's a questionable choice but at least it doesn't introduce new dependencies). 

# 🔥 Hot-Reloading with ghcid

The choice of binary commands to interface with JavaScript has following drawbacks and benefits:
1) Makes harder to FFI with JavaScript (you have to construct Lambda-style AST instead of writing more natural JavaScript)
2) It makes possible to run client apps with Native GHC executables 
3) It makes much easier to implement hot-reloading because the commands can be passed through a wire, other stuff works exacly the same way


# 🐞 WebAssembly Backend Bug

Unfortunately I encountered a bug in WebAssembly Backend — reactor module crashes after several GC sweeps. To counteract that I tweak RTS options (greadymem examples) that prolong application lifetime in exchange for high memory consumption. Hopefully the bug will be addressed in near future

# 💡 Examples

<table>
  <tbody>
    <tr>
      <td>Simple Voting</td>
      <td><a href=./examples/voting/voting.hs target=_blank>source</a></td>
      <td>
        <a href=https://lagunoff.github.io/htmt-wasm/examples/voting.html target=_blank>open</a>
      </td>
      <td>
        <a href=https://lagunoff.github.io/htmt-wasm/examples/voting-greadymem.html target=_blank>greadymem hack</a>
      </td>
    </tr>
    <tr>
      <td>TodoMVC</td>
      <td><a href=./examples/todomvc/todomvc.hs target=_blank>source</a></td>
      <td>
        <a href=https://lagunoff.github.io/htmt-wasm/examples/todomvc.html target=_blank>open</a>
      </td>
      <td>
        <a href=https://lagunoff.github.io/htmt-wasm/examples/todomvc-greadymem.html target=_blank>greadymem hack</a>
      </td>
    </tr>
  </tbody>
</table>
