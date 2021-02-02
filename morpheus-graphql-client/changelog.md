# Changelog

## 0.17.0 - 25.02.2021

### Breaking changes

- `GQLScalar` was replaced with `EncodeScalar` and `DecodeScalar` type-classes.

### Minor Changes

- more likely to rebuild when a file loaded by `defineByDocumentFile` or
  `defineByIntrospectionFile` is changed
- response errors should always result in a fetch failure [#577](https://github.com/morpheusgraphql/morpheus-graphql/issues/577)

## 0.16.0 - 05.11.2020

### Minor Changes

- parser performance optimization
- fixed #514: json parser selects interface type as fallback
- fixed #546: `defineByIntrospectionFile` support custom (Query,Mutation,Subscription)

## 0.15.1 - 12.09.2020

## 0.15.0 - 12.09.2020

### minor

- client capitalizes type names [#519](https://github.com/morpheusgraphql/morpheus-graphql/issues/519)

- fixed client error on field `__typename` [#509](https://github.com/morpheusgraphql/morpheus-graphql/issues/509)

## 0.14.1 - 16.08.2020

### minor

- fixed Build error during testing [#5602](https://github.com/commercialhaskell/stackage/issues/5602)

## 0.14.0 - 15.08.2020

### new features

- supports interfaces.

- supports of block string values.

- support of `schema`. issue #412

  ```graphql
  schema {
    query: MyQuery
  }
  ```

- generated types have instance of class `Eq`

### breaking changes

- custom scalars Should Provide instance of class `Eq`

## 0.13.0 - 22.06.2020

### breaking changes

- from now you should provide for every custom graphql scalar definition corresponding haskell type definition and `GQLScalar` implementation fot it. for details see [`examples-client`](https://github.com/morpheusgraphql/morpheus-graphql/tree/master/examples-client)

- input fields and query arguments are imported without namespace

### new features

- exposed: `ScalarValues`,`GQLScalar`, `ID`

## 0.12.0 - 21.05.2020
