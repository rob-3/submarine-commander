# Captain Sonar Helper

## Getting Started

### Docker

Clone the repository.

```bash
git clone https://github.com/rob-3/captain-sonar-helper.git
cd captain-sonar-helper
```
Build the container.

```bash
docker build -t captain-sonar .
```

Run the container.

```bash
docker run -v .:/code/ -p12345:12345 -p3000:3000 -i captain-sonar:latest devbox run clojure -M:nrepl-docker
```

This will start up a Clojure [nrepl](https://github.com/nrepl/nrepl), from which you can start the application. Type `(-main)` into the REPL and press enter.

```clojure
captain-sonar.captain-sonar=> (-main)
```

## License

MPL-2.0, see LICENSE
