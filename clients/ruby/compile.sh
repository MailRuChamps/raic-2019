set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/my_strategy.rb my_strategy.rb
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

ruby -c main.rb
cp -r * /output/