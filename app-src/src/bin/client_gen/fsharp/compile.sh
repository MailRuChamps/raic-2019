set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/MyStrategy.fs MyStrategy.fs
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

dotnet publish -c Release -o /output/