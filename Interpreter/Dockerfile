FROM mcr.microsoft.com/dotnet/runtime:7 AS base
WORKDIR /app

USER app
FROM mcr.microsoft.com/dotnet/sdk:7 AS build
ARG configuration=Release
WORKDIR /src
COPY ["Interpreter.csproj", "./"]
RUN dotnet restore "Interpreter.csproj"
COPY . .
WORKDIR "/src/."
RUN dotnet build "Interpreter.csproj" -c $configuration -o /app/build

FROM build AS publish
ARG configuration=Release
RUN dotnet publish "Interpreter.csproj" -c $configuration -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Interpreter.dll"]