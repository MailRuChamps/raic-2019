set -ex

if [ "$1" != "base" ]; then
    if [[ `ls -1 /src/ | wc -l` -eq 1 ]]; then
        cp -f /src/my_strategy.py my_strategy.py
    else
        rm -rf ./*
        cp -rf /src/* ./
    fi
fi

find . -name '*.pyx' -exec cythonize -i {} \;
python -m py_compile main.py
cp -r * /output/