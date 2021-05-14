# Changelog

## Unreleased

- The parser is compliant with the GQL specification and supports all valid characters #581
- The parser performance improvement: on average 3-4 times faster, in some cases more than 100 times faster.

## 0.17.0 - 25.02.2021

### New features

- `Data.Morpheus.Core` provides default GrapHQL type definitions with `internalSchema`
- exposed `Data.Morpheus.Internal.Ext`

### Breaking changes

- `parseTypeSystemDefinition` and `parseGQLDocument` is replaced with `parseSchema`
- `parseFullGQLDocument` replaced with `parseFullSchema`
- removed `parseDSL` from `Data.Morpheus.Core`

- following Types and modules are migrated to the new package `morpheus-graphql-app`:

  - following types and functions in `Data.Morpheus.Core` are moved in to `Data.Morpheus.App`:
    `App`, `AppData`, `runApp`, `withDebugger`, `mkApp`, `runAppStream`
  - typeClass `MapAPI` migrated from `Data.Morpheus.Types.IO` is moved into `Data.Morpheus.App`
  - `Data.Morpheus.Types.Internal.Resolving` moved as`Data.Morpheus.App.Internal.Resolving`

- `RootResModel` was renamed to `RootResolverValue`
- `ResModel` was replaced with more general `ResolverValue`
- `GQLScalar` was replaced with `EncodeScalar` and `DecodeScalar` type-classes.

- `Value.Float` is now `Double` instead of `Float`.

## 0.16.0 - 05.11.2020

### Breaking Changes

- signature changes:

  - `render`:
    `a -> Text`
    to `a -> ByteString`
  - parseTypeSystemDefinition :
    `Text -> Eventless (Schema VALID)`
    to `ByteString -> Eventless (Schema VALID)`

  - parseTypeDefinitions:
    `Text -> Eventless [TypeDefinition ANY CONST]`
    to `ByteString -> Eventless [TypeDefinition ANY CONST]`

### new features

### Minor Changes

- parser performance optimization

## 0.15.1 - 12.09.2020

relaxed upper boundary of `megaparsec` up to 10.0.0

## 0.15.0 - 12.09.2020

### new features

- `render` renders SchemaDefinition e.g

  ```graphql
  schema {
    query: MyQuery
  }
  ```

- query validator automatically adds `__typename` to interface types

- type : `App`

  ```hs
  api :: a -> m b
  api = runApp (mkApp schema resolvers)
  ```

- `App` supports semigroup(`schema Stitching`):

  if whe have two apps `app1` and `app2` with type `Api EVENT IO` we can merge it as.

  ```hs
  mergedApi :: a -> m b
  mergedApi = runApp (app1 <> app2)
  ```

- `runApp` changed signature to:

  ```hs
  runApp :: Api e m -> a -> m b
  ```

### Breaking Changes

- removed `runApi`.

### Minor Changes

- internal refactoring

## 0.14.1 - 16.08.2020

## 0.14.0 - 15.08.2020

### new features

- query validation supports interfaces
- exposed: `Data.Morpheus.Types.SelectionTree`
- configurable api: `Data.Morpheus.Core` exports

  - `Config`
  - `defaultConfig`
  - `debugConfig`

- for better debugging, internal errors messages will display resolving state:
  - `current TypeName`
  - `current Selection`
  - `OperationDefinition`
  - `SchemaDefinition`
- rendering graphql "AST". e.g `render (selection :: Selection VALID)` will render

```graphql
{
  user(arg1: 1) {
    name
  }
}
```

- quasiquoter `[dsl| <type definitions> |]` generates `Schema VALID`.
- parser supports custom directive definition. e.g

```graphql
directive @MyDirective on FIELD_DEFINITION | OBJECT
```

- directive Validation for Document (TypeSystem).
- supports of block string values. e.g:

  ```graphql
  query {
    createDeity(
      name: """
      power
      bla \n sd
      blu \\ date
      """
    ) {
      name
    }
  }
  ```

- support of `schema`. issue #412

  ```graphql
  schema {
    query: MyQuery
  }
  ```

### Breaking Changes

- `Context' renamed to`ResolverContext'
- removed : `EventCon` from `Data.Morpheus.Core`
- internal refactoring: changed AST.
  Schema AST Types now need parameter `stage = RAW | CONST | VALID`.
  - `Schema VALID`
  - `TypeDefinition VALID`
  - `FieldDefinition IN VALID`
  - ...
- runApi requires argument config

  ```hs
    runApi ::
      Schema s ->
      RootResModel event m ->
      Config ->
      GQLRequest ->
      ResponseStream event m (Value VALID)
  ```

## 0.13.0 - 22.06.2020

### new features

- exposed: `Data.Morpheus.Types.GQLScalar`
- exposed: `Data.Morpheus.Types.ID`
- finished interface validation
- supports default values

## minor changes

- internal refactoring
- added dependency `mtl`
- validates strings as enum from JSON value

## 0.12.0 - 21.05.2020

## New features

- parser supports implements interfaces separated with empty spaces

  ```gql
  type T implements A , B C & D {
  ```

- introspection can render interfaces
