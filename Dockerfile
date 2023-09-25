FROM mcr.microsoft.com/dotnet/aspnet:7.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /src
COPY ["src/Interpreter/Interpreter.fsproj", "./"]
RUN dotnet restore "Interpreter.fsproj"
COPY ./src/Interpreter/ ./
WORKDIR "/src/"
RUN dotnet build "Interpreter.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "Interpreter.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Interpreter.dll", "/var/rinhas/"]