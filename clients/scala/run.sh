set -ex

cd /output
java -Xmx250m -jar ./aicup2019-assembly-1.0-SNAPSHOT.jar "$@"