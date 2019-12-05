set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/my-strategy.js my-strategy.js
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

cp -r * /output/