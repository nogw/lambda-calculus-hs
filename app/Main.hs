module Main where

import Data.Map as Map

type Context = Map String Value

data Expr
  = Var String
  | Abs {param :: String, body :: Expr}
  | App {fun :: Expr, arg :: Expr}
  | Lit Int
  | Prim Binop Expr Expr
  deriving (Show)

data Binop
  = Add
  | Subt
  deriving (Show)

data Value
  = VInt Int
  | Closure {context :: Context, param' :: String, body' :: Expr}
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
                  VInt v -> Just (VInt v)
                _ -> Nothing
    Lit n -> Just (VInt n)
    Prim op v v' ->
      case (interprete context v, interprete context v') of
        (Just v, Just v') -> Just (interpreteBinop op v v')
        _ -> interprete context v

interpreteBinop :: Binop -> Value -> Value -> Value
interpreteBinop op (VInt v) (VInt v') = case op of
  Add -> VInt $ v + v'
  Subt -> VInt $ v - v'
interpreteBinop op _ _ = error "only VInt"

idL :: Expr
idL = Abs {param = "x", body = Var "x"}

callIdL :: Expr
callIdL = App {fun = idL, arg = Abs {param = "y", body = Var "y"}}

lambSum :: Int -> Int -> Expr
lambSum x y =
  App
    ( App
        ( Abs
            "x"
            (Abs 
              "y" 
              (Prim Add (Var "x") (Var "y"))
            )
        )
        (Lit x)
    )
    (Lit y)

sumL :: Int -> Int -> Expr
sumL x y =
  App
    { fun =
        App
          { fun =
              Abs
                { param = "x",
                  body =
                    Abs
                      { param = "y",
                        body = Prim Add (Var "x") (Var "y")
                      }
                },
            arg = Lit x
          },
      arg = Lit y
    }

main :: IO ()
main = print (interprete Map.empty (sumL 3 2))