# Dependencies
- stack
- npm
  + bower
  + pulp

# How to build

## Build the server

```
> stack setup
> stack build
```

## Generate the purescript code for the servant API

```
> stack exec psGenerator
```

## Build the frontend

```
> cd frontend
> npm install
> npm run build
```

## Run the server

```
> stack exec central-counter
> x-www-browser http://localhost:8081/index.html
```
