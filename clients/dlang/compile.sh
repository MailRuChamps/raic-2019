set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/my_strategy.d source/my_strategy.d
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

dub build -b release
cp aicup2019 /output/