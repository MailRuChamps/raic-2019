FROM mcr.microsoft.com/dotnet/core/sdk:3.0

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        curl \
		libunwind8 \
        wget \
        gpg \
        apt-transport-https \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update

RUN wget https://dot.net/v1/dotnet-install.sh && chmod +x dotnet-install.sh 

RUN ./dotnet-install.sh -Channel LTS && ./dotnet-install.sh -Channel Current

ENV PATH="/root/.dotnet:$PATH"