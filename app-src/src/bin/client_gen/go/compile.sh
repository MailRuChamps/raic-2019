set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/my_strategy.go my_strategy.go
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

go build -o aicup2019
cp aicup2019 /output/