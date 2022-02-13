module Main where

import Data.Map as Map

type Context = Map String Value

data Expr
  = Var String
  | Abs {param :: String, body :: Expr}
  | App {fun :: Expr, arg :: Expr}
  deriving (Show)

instance Show (a -> b) where
  show a = "TODO"

data Value = Closure {context :: Context, param' :: String, body' :: Expr}
  deriving (Show)

interprete :: Context -> Expr -> Maybe Value
interprete context expr =
  case expr of
    Var name -> Map.lookup name context
    Abs {param = param, body = body} -> Just $ Closure {context = context, param' = param, body' = body}
    App {fun = fun, arg = arg} ->
      let fun' = interprete context fun
       in let arg' = interprete context arg
           in case fun' of
             Just x -> case x of 
              Closure {context = context, param' = param, body' = body} -> case arg' of
                Just arg -> interprete (Map.insert param arg context) body
                _ -> Nothing 
             _ -> Nothing 

idL :: Expr
idL = Abs {param = "x", body = Var "x"}

callIdL :: Expr
callIdL = App {fun = idL, arg = Abs {param = "y", body = Var "y"}}

main :: IO (Maybe Value)
main = pure $ interprete Map.empty callIdL