set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/MyStrategy.scala src/main/scala/MyStrategy.scala
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

mvn package --batch-mode
cp target/aicup2019-jar-with-dependencies.jar /output/