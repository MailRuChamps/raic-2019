set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /MyStrategy.fs MyStrategy.fs
    else
        rm -rf ./*
        cp -rf /* ./
    fi
fi

dotnet build -c Release
cp bin/Release/netcoreapp3.0/* /output/