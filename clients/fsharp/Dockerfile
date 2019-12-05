FROM debian

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    curl \
    libunwind8 \
    wget \
    gpg \
    apt-transport-https \
    ca-certificates \
    zlib1g libicu63 libcurl4
RUN wget https://dot.net/v1/dotnet-install.sh && chmod +x dotnet-install.sh
RUN ./dotnet-install.sh -Channel 2.1 && ./dotnet-install.sh -Channel 3.0 && ./dotnet-install.sh -Channel 3.1

ENV PATH="/root/.dotnet:$PATH"

COPY . /project
WORKDIR /project

RUN dotnet build -c Release