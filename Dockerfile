FROM node as builder
RUN unset NODE_ENV
WORKDIR /src
COPY package.json .
COPY package-lock.json .
COPY elm.json .
RUN npm install
COPY vite.config.js .
COPY index.html .
COPY public .
COPY src .
RUN npm run build

FROM nginxinc/nginx-unprivileged:stable-alpine
COPY --from=builder /src/dist /usr/share/nginx/html
EXPOSE 8080
