FROM debian

RUN apt-get update && apt-get install -y libasound2 libgtk-3-0

COPY aicup2019 aicup2019