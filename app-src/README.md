To build, you'll need to install [Rust](https://rustup.rs/).

To run native build:

```shell
cargo run --release
```

To build web version, first install [`cargo-web`](https://github.com/koute/cargo-web):

```shell
cargo install cargo-web
```

Then build, start local server and open browser:

```shell
cargo web start --release --open
```