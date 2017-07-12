module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (fold, intercalate)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Field, Product, Rec)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- GArray because Gary said I can't have Generic Array
newtype GArray a = GArray (Array a)
instance genericGArray
  :: Generic (GArray a) (Constructor "Array" (Argument a)) where
  to _ = GArray [] -- you can see this is a lie
  from _ = Constructor (Argument (undefined)) -- i think it's fine though
    where
      undefined = unsafeCoerce unit

newtype Unused = Unused String
derive instance genericUnused :: Generic Unused _

newtype Flag = Flag Boolean
derive instance genericFlag :: Generic Flag _

newtype Path = Path String
derive instance genericPath :: Generic Path _

newtype FileData = FileData
  { path :: Path
  , watched :: Flag
  }
derive instance genericFileData :: Generic FileData _

data Method
  = GET
  | POST
derive instance genericMethod :: Generic Method _
instance showMethod :: Show Method where
  show = genericShow

newtype Route req res = Route
  { method :: Method
  , url :: String
  }
derive instance genericRoute :: Generic (Route req res) _

files :: Route Unused (GArray Path)
files = Route
  { method: GET
  , url: "/api/files"
  }

watched :: Route Unused (GArray FileData)
watched = Route
  { method: GET
  , url: "/api/watched"
  }

nameOf :: forall a rep
  . Generic a rep
  => GTypeName rep
  => Proxy a
  -> String
nameOf _ = gTypeName (Proxy :: Proxy rep)

class GTypeName rep where
  gTypeName :: forall f. f rep -> String
  
instance gTypeNameArray ::
  ( GTypeName a
  ) => GTypeName (Constructor "Array" a) where
  gTypeName _ = gTypeName (Proxy :: Proxy a) <> "[]"

instance gTypeNameConstructor ::
  ( IsSymbol f
  ) => GTypeName (Constructor f a) where
  gTypeName _ = reflectSymbol (SProxy :: SProxy f)

instance gTypeNameArgument ::
  ( GTypeName rep
  , Generic f rep
  )
  => GTypeName (Argument f) where
  gTypeName _ = gTypeName (Proxy :: Proxy rep)

instance gTypeNameString :: GTypeName String where
  gTypeName _ = "string"

instance gTypeNameBoolean :: GTypeName Boolean where
  gTypeName _ = "boolean"

writeRouteDefinition :: forall req res a b
  . Generic req a
  => Generic res b
  => GTypeName a
  => GTypeName b
  => String
  -> Route req res
  -> String
writeRouteDefinition name route@Route{method, url} =
  "const " <> name
    <> ": Route<" <> reqName <> ", " <> resName <>  "> = {\n"
    <> "  method: \"" <> method' <> "\",\n"
    <> "  url: \"" <> url <> "\"\n"
    <> "}\n"
  where
    reqName = nameOf (Proxy :: Proxy req)
    resName = nameOf (Proxy :: Proxy res)
    method' = show method

class ExtractFields rep where
  extractFields :: forall f. f rep -> Array (Tuple String String)

instance constructorExtractFields ::
  ( ExtractFields a
  )
  => ExtractFields (Constructor sym a) where
  extractFields _ = extractFields (Proxy :: Proxy a)

instance argumentExtractFields ::
  ( GTypeName a
  )
  => ExtractFields (Argument a) where
  extractFields _ =
    pure <<< Tuple "" $
      gTypeName (Proxy :: Proxy a)

instance fieldExtractFields ::
  ( IsSymbol field
  , Generic a rep
  , GTypeName rep
  )
  => ExtractFields (Field field a) where
  extractFields _ =
    pure $ Tuple
      (reflectSymbol (SProxy :: SProxy field))
      (gTypeName (Proxy :: Proxy rep))

instance recExtractFields ::
  ( ExtractFields fields
  )
  => ExtractFields (Rec fields) where
  extractFields _ =
    extractFields (Proxy :: Proxy fields)

instance productExtractFields ::
  ( ExtractFields a
  , ExtractFields b
  )
  => ExtractFields (Product a b) where
  extractFields _ =
    extractFields (Proxy :: Proxy a) <> extractFields (Proxy :: Proxy b)

writeTypeDefinition :: forall a rep
  . Generic a rep
  => GTypeName rep
  => ExtractFields rep
  => Proxy a
  -> String
writeTypeDefinition proxy =
  case fst <$> fields of
    [""] -> -- simple newtype check
      "type " <> name <> " = "
        <> (fold $ snd <$> fields)
        <> " // this is a LIE\n"
    _ ->
      "type " <> name <> " = {\n"
        <> contents
        <> "\n}\n"
  where
    name = nameOf proxy
    fields = extractFields (Proxy :: Proxy rep)
    format (Tuple key prop) = "  " <> key <> ": " <> prop
    contents =
      intercalate "\n" $
        format <$> fields

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ writeRouteDefinition "files" files
  log $ writeRouteDefinition "watched" watched
  log $ writeTypeDefinition (Proxy :: Proxy Path)
  log $ writeTypeDefinition (Proxy :: Proxy Flag)
  log $ writeTypeDefinition (Proxy :: Proxy FileData)
