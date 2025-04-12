FROM jetpackio/devbox:latest

WORKDIR /code
USER root:root
COPY devbox.json devbox.lock ./
RUN devbox install

COPY . .
RUN devbox run clojure -T:build ci

CMD ["devbox", "run", "java", "-jar", "target/net.clojars.submarine-commander/submarine-commander-0.1.0-SNAPSHOT.jar"]
