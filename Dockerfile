FROM jetpackio/devbox:latest

# devbox things
WORKDIR /code
USER root:root
RUN mkdir -p /code && chown ${DEVBOX_USER}:${DEVBOX_USER} /code
USER ${DEVBOX_USER}:${DEVBOX_USER}
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} devbox.json devbox.json
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} devbox.lock devbox.lock

RUN devbox install

COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} . .

RUN devbox run clojure -T:build ci

CMD ["devbox", "run", "java", "-jar", "target/net.clojars.captain-sonar/captain-sonar-0.1.0-SNAPSHOT.jar"]
