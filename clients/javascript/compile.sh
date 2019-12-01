if [ "$1" == "" ]; then
    echo This script is used for compiling on the server
    exit 1
fi

set -ex

if [ "$1" != "base" ]; then
    rm "my-strategy.js"
    cp -rn /src/* ./
fi

cp -r * /output/
