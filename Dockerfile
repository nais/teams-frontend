FROM node as builder

WORKDIR /src
COPY package.json package-lock.json elm.json ./
RUN npm install

COPY vite.config.js index.html ./
COPY ./public ./public
COPY ./src ./src
RUN ls -l
RUN npm run build

FROM nginxinc/nginx-unprivileged:stable-alpine
COPY --from=builder /src/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf
EXPOSE 8080
