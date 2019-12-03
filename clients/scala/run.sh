set -ex

cd /output
java -Xmx250m -jar ./aicup2019-jar-with-dependencies.jar "$@"