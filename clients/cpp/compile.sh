set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/MyStrategy.cpp MyStrategy.cpp
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

cmake -DCMAKE_CXX_STANDARD=17 -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_VERBOSE_MAKEFILE=ON .
cmake --build . --config Release
cp aicup2019 /output/