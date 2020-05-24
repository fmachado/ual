module ParseTypes (ParseResult (..), thenP, returnP)

where

data ParseResult a
      = ParseOk a
      | ParseFail String

type P a = String -> Integer -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l ->
      case m s l of
              ParseFail s -> ParseFail s
              ParseOk a -> k a s l

returnP :: a -> P a
returnP a = \s l -> ParseOk a
