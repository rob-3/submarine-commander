# Submarine Commander

## Getting Started

### Docker

Clone the repository.

```bash
git clone https://github.com/rob-3/submarine-commander.git
cd submarine-commander
```
Build the container.

```bash
docker build -t submarine-commander .
```

Run the container.

```bash
docker run -v .:/code/ -p12345:12345 -p3000:3000 -i submarine-commander:latest devbox run java -jar target/net.clojars.submarine-commander/submarine-commander-0.1.0-SNAPSHOT.jar
```

## License

MPL-2.0, see LICENSE
