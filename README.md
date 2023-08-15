# NAIS Teams frontend

## Developing

```bash
npm install
npm run dev
```

There are additional dependencies on nais/teams-backend which you may want to handle.

For a combination of more tools running locally ([hookd](https://github.com/nais/deploy), [NAIS Teams frontend](https://github.com/nais/teams-frontend) and more), check out the [nais/features-dev](https://github.com/nais/features-dev) repo.

### Updated GraphQL schema

Whenever `teams-backend` updates its GraphQL schema you might want to update the models. This can be done by running the following command:

```bash
npm run gqlgen
```

This requires that you have a running instance of `teams-backend` on `localhost:3000`.