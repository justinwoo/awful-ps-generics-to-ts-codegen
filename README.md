# awful-ps-generics-to-ts-codegen

Demo generation of Typescript code using information about Purescript types using Generics-Rep. Similar idea to [godawful-purescript-codegen-demo](https://github.com/justinwoo/godawful-codegen-demo).

Unfortunately, it doesn't seem like much can actually be done with the phantom type data that would be passed to Route in this case other than to start generating things like `interface Decode<T> { parseJSON: string => Either<Error, T>` } or some painful approximation.

One thing I did here different was to require all fields to be Generics themselves, while the Haskell version takes advantage of type literals, data kinds, flexible contexts, etc. Well, not like Purescript couldn't eventually adopt these too (it probably will?).

The `Route` type below should be defined as something like the following:

```ts
type Route<Req, Res> = {
  method: 'GET' | 'POST'
  url: string
}
```

Example output:

```ts
const files: Route<Unused, Path[]> = {
  method: "GET",
  url: "/api/files"
}

const watched: Route<Unused, FileData[]> = {
  method: "GET",
  url: "/api/watched"
}

type Path = string // this is a LIE

type Flag = boolean // this is a LIE

type FileData = {
  path: Path
  watched: Flag
}
```