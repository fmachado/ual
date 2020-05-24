> import IO ( openFile, hGetContents )
> import PrelHandle
> import EvalProgram ( execProg )
> import System ( getProgName, getArgs )
> import CheckProgram
> import Grammar (lexer, grammar)
> import EvalExp
> import UalTypes
> import ParseTypes
> main = do
>          argv0 <- getProgName
>          [arquivo] <- getArgs
>          fdin <-openFile arquivo ReadMode
>          programa <- hGetContents fdin
>          let (ParseOk (Program v ts cs)) = grammar programa 1
>          ts'     <- checkProgram (Program v ts cs)
>          garbage <- fillGarbage (removeVar ts) []
>          execProg (Program v ts cs) garbage
