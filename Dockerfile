FROM cgr.dev/chainguard/node as builder

WORKDIR /src
COPY --chown=node:node package.json package-lock.json elm.json ./
RUN npm install

COPY --chown=node:node vite.config.js index.html ./
COPY --chown=node:node ./public ./public
COPY --chown=node:node ./src ./src
RUN ls -l
RUN npm run build

FROM cgr.dev/chainguard/nginx:latest
COPY --from=builder /src/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf
EXPOSE 8080
