> import IO ( openFile, hGetContents )
> import PrelHandle
> import DaVinci
> import System ( getProgName, getArgs )
> import GenGraph ( execGraph, annProg )
> import Grammar (lexer, grammar)
> import CheckProgram
> import EvalExp
> import UalTypes
> import ParseTypes
> main = do
>          argv0 <- getProgName
>          [arquivo,t] <- getArgs
>          fdin <-openFile arquivo ReadMode
>          programa <- hGetContents fdin
>          let (ParseOk (Program v ts cs)) = grammar programa 1
>          ts'     <- checkProgram (Program v ts cs)
>          (n,ann) <- annProg (Program v ts cs)
>          let n = read t
>          m <- conf n
>          dvpid <- startDaVinci False
>          garbage <- fillGarbage (removeVar ts) []
>          p <- execGraph ann dvpid garbage ts m
>          stopDaVinci dvpid
> conf t = if t > 86600 then error "Faixa de tempo fora dos limites!"
>                       else return t
