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
docker run -p3000:3000 -i submarine-commander:latest
```

## License

MPL-2.0, see LICENSE
