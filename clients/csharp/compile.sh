set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/MyStrategy.cs MyStrategy.cs
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

dotnet build -c Release
cp bin/Release/netcoreapp2.1/* /output/