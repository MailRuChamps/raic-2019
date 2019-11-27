set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/my_strategy.rs src/my_strategy.rs
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

cargo build --release --offline
cp target/release/aicup2019 /output/