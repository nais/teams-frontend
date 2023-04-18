FROM cgr.dev/chainguard/node as builder

COPY --chown=node:node package.json package-lock.json elm.json ./
RUN npm install

COPY --chown=node:node vite.config.js index.html ./
COPY --chown=node:node ./public ./public
COPY --chown=node:node ./src ./src
RUN ls -l
RUN npm run build

FROM cgr.dev/chainguard/nginx:latest
COPY --from=builder /app/dist /var/lib/nginx/html/
EXPOSE 80
