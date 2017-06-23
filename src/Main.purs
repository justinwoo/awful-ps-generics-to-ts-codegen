module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (head)
import Data.Foldable (intercalate, length)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Field, Product, Rec, from)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typeable (class Typeable, TyCon(..), TypeRep(..), typeOf, typeRepTyCon)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

mkTyCon' :: String -> TypeRep
mkTyCon' tyConName = TypeRep
  (TyCon {tyConModule: "Main", tyConName})
  mempty

newtype Unused = Unused String
derive instance genericUnused :: Generic Unused _

newtype Flag = Flag Boolean
derive instance genericFlag :: Generic Flag _
instance typeableFlag :: Typeable Flag where
  typeOf _ = mkTyCon' "Flag"

newtype Path = Path String
derive instance genericPath :: Generic Path _
instance typeablePath :: Typeable Path where
  typeOf _ = mkTyCon' "Path"

newtype FileData = FileData
  { path :: Path
  , watched :: Flag
  }
derive instance genericFileData :: Generic FileData _
instance typeableFileData :: Typeable FileData where
  typeOf _ = mkTyCon' "FileData"

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

-- GArray because Gary said I can't have Generic Array
newtype GArray a = GArray (Array a)
instance genericGArray
  :: Generic (GArray a) (Constructor "Array" (Argument a)) where
  to _ = GArray [] -- you can see this is a lie
  from _ = Constructor (Argument (undefined)) -- i think it's fine though

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
nameOf _ = gtypeName (from (undefined :: a))

class GTypeName rep where
  gtypeName :: rep -> String
  
instance gTypeNameConstructor ::
  ( IsSymbol f
  , GTypeName a
  )
  => GTypeName (Constructor f a) where
  gtypeName _ = case reflectSymbol (SProxy :: SProxy f) of
    "Array" ->
      gtypeName (undefined :: a) <> "[]"
    x -> x

instance gTypeNameArgument ::
  ( Typeable f
  )
  => GTypeName (Argument f) where
  gtypeName _ = show <<< typeRepTyCon $ typeOf (Proxy :: Proxy f)

instance gTypeNameRec :: GTypeName (Rec a) where
  gtypeName _ = "only here to satisfy compiler"

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
    <> "  method: " <> method' <> ",\n"
    <> "  url: \"" <> url <> "\"\n"
    <> "}\n"
  where
    reqName = nameOf (Proxy :: Proxy req)
    resName = nameOf (Proxy :: Proxy res)
    method' = show method

class ExtractFields rep where
  extractFields :: rep -> Array (Tuple String String)

instance constructorExtractFields ::
  ( ExtractFields a
  )
  => ExtractFields (Constructor sym a) where
  extractFields _ = extractFields (undefined :: a)

instance argumentExtractFields ::
  ( GTypeName (Argument a)
  )
  => ExtractFields (Argument a) where
  extractFields _ =
    pure <<< Tuple "" $ gtypeName (undefined :: Argument a)

instance fieldExtractFields ::
  ( IsSymbol field
  , Generic a rep
  , GTypeName rep
  )
  => ExtractFields (Field field a) where
  extractFields _ =
    pure $ Tuple
      (reflectSymbol (SProxy :: SProxy field))
      (gtypeName (undefined :: rep))

instance recExtractFields ::
  ( ExtractFields fields
  )
  => ExtractFields (Rec fields) where
  extractFields _ =
    extractFields (undefined :: fields)

instance productExtractFields ::
  ( ExtractFields a
  , ExtractFields b
  )
  => ExtractFields (Product a b) where
  extractFields _ =
    extractFields (undefined :: a) <> extractFields (undefined :: b)

writeTypeDefinition :: forall a rep
  . Generic a rep
  => GTypeName rep
  => ExtractFields rep
  => Proxy a
  -> String
writeTypeDefinition proxy =
  if length fields == 1 && (fst <$> head fields) == Just "" -- simple newtype check
  then
    "type " <> name <> " = "
      <> (fromMaybe "" $ snd <$> head fields)
      <> " // this is a LIE\n"
  else
    "type " <> name <> " = {\n"
      <> contents
      <> "\n}\n"
  where
    name = nameOf proxy
    fields = extractFields (from (undefined :: a))
    format (Tuple key prop) = "  " <> key <> ": " <> prop
    contents =
      intercalate "\n" $
        format <$> fields

undefined :: forall a. a
undefined = unsafeCoerce unit

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ writeRouteDefinition "files" files
  log $ writeRouteDefinition "watched" watched
  log $ writeTypeDefinition (Proxy :: Proxy Path)
  log $ writeTypeDefinition (Proxy :: Proxy FileData)


