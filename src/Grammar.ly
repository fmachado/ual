************************************************************************

 UAL - Unesa Algorithm Language
 Módulo Grammar.ly
 Descrição: Este módulo define a Gramática utilizada pela linguagem e
            o Analisador Léxico para a mesma.

************************************************************************

 O módulo abaixo contém sua identificação e as funções a serem exportadas
 e importadas.

> {
> module Grammar ( lexer, grammar, ParseResult )
> where
> import Char ( isAlpha, isDigit, isAlphaNum, isSpace )
> import ParseTypes
> import UalTypes
> }

 Abaixo estão declarados o nome do nosso Analisador Sintático e o tipo dos
 tokens por ele interpretados.
 
> %name grammar
> %tokentype { Token }

 A seguir, encontra-se a chamada ao monad utilizado.

> %monad { P } { thenP } { returnP }
> %lexer { lexer } { TK_EOF }

 Em seguida, temos a definição dos tokens que podem ser utilizados no
 programa. O lado esquerdo representa as palavras reservadas do programa
 e o lado direito, entre chaves, representa a forma pela qual o 
 interpretador se refere às palavras reservadas dentro do programa.

> %token
>        int            {TK_INT}
>        num            {TK_NUM $$}
>        var            {TK_ID $$}
>        real           {TK_REAL}
>        float          {TK_FLOAT $$}
>        boolean        {TK_BOOLEAN $$}
>        logico         {TK_BOOL}
>        str            {TK_STR $$}
>        string         {TK_STRING}
>        '<-'           {TK_ATRIB}
>        prog           {TK_PROG}
>        fimprog        {TK_ENDPROG}
>        leia           {TK_LEIA}
>        imprima        {TK_IMPRIMA}
>        formatar       {TK_FORMATAR}
>        ';'            {TK_PV}
>        '+'            {TK_MAIS}
>        '-'            {TK_MENOS}
>        '*'            {TK_PROD}
>        '/'            {TK_DIV}
>        'div'          {TK_IDIV}
>        '^'            {TK_IPOT}
>        ']'            {TK_FCOL}
>        '['            {TK_ACOL}
>        ','            {TK_VIRG}
>        '('            {TK_ABREPAR}
>        ')'            {TK_FECHAPAR}
>        '{'            {TK_ABRECHAV}
>        '}'            {TK_FECHACHAV}
>        '=='           {TK_IGUAL}
>        '>'            {TK_MAIOR}
>        '<'            {TK_MENOR}
>        '%'            {TK_RESTO}
>        '>='           {TK_MAIORIG}
>        '<='           {TK_MENORIG}
>        '<>'           {TK_DIF}
>        '&&'           {TK_E}
>        '||'           {TK_OU}
>        '!'            {TK_NAO}
>        '++'           {TK_INCR}
>        '--'           {TK_DECR}
>        '**'           {TK_POT}
>        se             {TK_SE}
>        senao          {TK_SENAO}
>        enquanto       {TK_ENQUANTO}
>        faca           {TK_FACA}
>        para           {TK_PARA}
>        strtam         {TK_STRCOMP}
>        strcomp        {TK_STRCOMPARA}
>        strult         {TK_STRULT}
>        strnprim       {TK_STRNPRIM}
>        strnresto      {TK_STRNRESTO}
>        strconcat      {TK_STRCONCAT}
>        strcopia       {TK_STRCOPIA}
>        strprim        {TK_STRPRIM}
>        strresto       {TK_STRCALDA}
>        strelem        {TK_STRELEM}
>        raiz           {TK_RAIZQUAD}
>        sen            {TK_SENO}
>        cos            {TK_COSENO}
>        tan            {TK_TANGENTE}
>        exp            {TK_EXP}
>        log            {TK_LOG}
>        pi             {TK_PI}
>        abs            {TK_ABS}
>        intreal        {TK_INTREAL}
>        realint        {TK_REALINT}
>        saia           {TK_SAIA}
>        continue       {TK_CONTINUE}
>        pare           {TK_PARE}

> %%

 Abaixo, introduzimos a notação das regras gramaticais para especificar a sintaxe da
 linguagem. 
 
 A gramática descreve, naturalmente, a estrutura hierárquica da construção
 da linguagem UAL. Essa estrutura encontra-se dividida em terminais e não terminais, 
 onde os tokens, isto é, palavras reservadas, parênteses, etc., são os terminais e os
 enunciados à esquerda, como program e typelist, representam seqüências de tokens, sendo
 chamados de não-terminais. 

 Com base nessas regras é realizada a análise hierárquica, também chamada de análise
 gramatical ou sintática. Essa análise envolve o agrupamento dos tokens do programa
 fonte em frases gramaticais, que são usadas pelo interpretador, a fim de sintetizar 
 a saída.
 
 Através da estrutura citada acima, é montada a árvore gramatical, que mostra 
 pictoricamente, como o símbolo de partida de uma gramática deriva uma cadeia de 
 linguagem. Desse modo, a árvore gramatical será composta pela raiz, que é rotulada pelo 
 símbolo de partida, das folhas, que são rotuladas por um token e dos nós interiores, 
 rotulados por um não-terminal.

> program :: { Program }
>         : prog var typelist statements fimprog
>                   {% \s l -> ParseOk (Program (Simple $2) $3 $4)}

> typelist :: { [TypeGram] }
>          :        { [] }
>          | type typelist
>                   { $1 : $2 }

> type :: { TypeGram }
>      : int vars ';'
>                   {VarInt $2}
>      | real vars ';'
>                   {VarReal $2}
>      | logico vars ';'
>                   {VarBool $2}
>      | string vars ';'
>                   {VarString $2}

> vars :: { [Var] }
>      : var   
>                  { [Simple $1] }
>      | var '[' num ']'
>                  { if $3 < 1 then error ("Índice incorreto na declaração do vetor " ++ $1 ++ ".")
>                               else [Vector $1 (Factor (E_Int $3))] }
>      | var ',' vars
>                   { Simple $1 : $3 }
>      | var '[' num ']' ',' vars
>                  { if $3 < 1 then error ("Índice incorreto na declaração do vetor " ++ $1 ++ ".")
>                               else Vector $1 (Factor (E_Int $3)) : $6 }

> atrib :: { Comand }
>       : var '<-' logop
>                   { Atrib (Simple $1) $3 }
>       | var '[' expression ']' '<-' logop
>                   { Atrib (Vector $1 $3) $6 }
>       | var '++'
>                   { E_Incr (Simple $1) }
>       | var '[' expression ']' '++'
>                   { E_Incr (Vector $1 $3) }
>       | var '--'
>                   { E_Decr (Simple $1) }
>       | var '[' expression ']' '--'
>                   { E_Decr (Vector $1 $3) }

> statements :: { [Comand] }
>            :      { [] }
>            | statement statements
>                   { $1 : $2 }

> statement :: { Comand }
>           : atrib ';'
>                   { $1 }
>           | se '(' logop ')' '{' statements '}'
>                   { E_Se $3 $6 }
>           | se '(' logop ')' '{' statements '}' senao '{' statements '}'
>                   { E_Senao $3 $6 $10 }
>           | enquanto '(' logop ')' '{' statements '}'
>                   { E_Enquanto $3 $6 }
>           | faca '{' statements '}' enquanto '(' logop ')'
>                   { E_Faca $3 $7 }
>           | para '(' atrib ';' logop ';' atrib ')' '{' statements '}'
>                   { E_Para $3 $5 $7 $10 }
>           | leia readexp ';'
>                   { E_Leia $2 }
>           | imprima expressions ';'
>                   { E_Imprima $2 }
>           | saia ';'
>                   { E_Saia }
>           | continue ';'
>                   { E_Continue }
>           | pare ';'
>                   { E_Pare }

> relop :: { Exp }
>       : expression 
>                   { $1 }
>       | relop '==' expression 
>                   { E_Igual $1 $3 }
>       | relop '>' expression
>                   { E_Maior $1 $3 }
>       | relop '<' expression
>                   { E_Menor $1 $3 }
>       | relop '>=' expression
>                   { E_Maiorig $1 $3 }
>       | relop '<=' expression
>                   { E_Menorig $1 $3 }
>       | relop '<>' expression
>                   { E_Dif $1 $3 }

> logop :: { Exp }
>       : relop
>                   { $1 }
>       | relop '&&' logop 
>                   { E_E $1 $3 }
>       | relop '||' logop 
>                   { E_Ou $1 $3 }
>       | '!' logop
>                   { E_Nao $2 }

> expressions :: { [Exp] }
>             : expression
>                   { [$1] }
>             | expression ',' expressions
>                   { $1 : $3 }

> expression :: { Exp }
>            : expression2
>                   { $1 }
>            | expression '+' expression2
>                   { E_Mais $1 $3 }
>            | expression '-' expression2
>                   { E_Menos $1 $3 }

> expression2 :: { Exp }
>             : expression3 
>                   { $1 }
>             | expression2 '*' expression3
>                   { E_Prod $1 $3 }
>             | expression2 '/' expression3
>                   { E_Div $1 $3 }
>             | expression2 'div' expression3
>                   { E_IDiv $1 $3 }


> expression3 :: { Exp }
>             : factor 
>                   { Factor $1 }
>             | expression3 '%' factor
>                   { E_Resto $1 $3 }
>             | expression3 '**' factor
>                   { E_Pot $1 $3}
>             | expression3 '^' factor
>                   { E_IPot $1 $3}

> readexp :: { Exp }
>         : var
>                   { Factor (E_Var $1) }
>         | var '[' expression ']'
>                   { Factor (E_Vector $1 $3) }

> factor :: { Factor }
>        : num
>                   { E_Int $1 }
>        | var
>                   { E_Var $1 }
>        | var '[' expression ']'
>                   { E_Vector $1 $3 }
>        | float 
>                   { E_Float $1 }
>        | boolean  
>                   { E_Boolean $1 }
>        | str   
>                   { E_Str $1 }
>        | '(' logop ')'
>                   { E_Par $2 }
>        | '+' factor
>                   { E_Pos $2 }
>        | '-' factor
>                   { E_Neg $2 }
>        | strtam  '(' factor ')' 
>                   { E_StrComp $3 }
>        | strconcat '(' factor ',' factor ')' 
>                   { E_StrConcat $3 $5 }
>        | strcopia '(' factor ')' 
>                   { E_StrCopia $3 }
>        | strult  '(' factor ')' 
>                   { E_StrUlt $3 }
>        | strcomp  '(' factor ',' factor ')' 
>                   { E_StrCompara $3 $5 }
>        | strprim '(' factor ')' 
>                   { E_StrPrim $3 }
>        | strnprim '(' factor ',' factor ')' 
>                   { E_StrNPrim $3 $5 }
>        | strresto '(' factor ')' 
>                   { E_StrCalda $3 }
>        | strnresto '(' factor ',' factor ')' 
>                   { E_StrNResto $3 $5}
>        | strelem '(' factor ',' factor ')' 
>                   { E_StrElem $3 $5 }
>        | raiz    '(' expression ')' 
>                   { E_RaizQuad $3 }
>        | sen '(' expression ')'
>                   { E_Seno $3 }
>        | cos '(' expression ')'
>                   { E_Coseno $3 }
>        | tan '(' expression ')'
>                   { E_Tangente $3 }
>        | exp '(' expression ')'
>                   { E_Exp $3 }
>        | log '(' expression ')'
>                   { E_Log $3 }
>        | pi
>                   { E_Pi }
>        | abs '(' expression ')'
>                   { E_Abs $3 }
>        | intreal '(' expression ')'
>                   { E_IntReal $3 }
>        | realint '(' expression ')'
>                   { E_RealInt $3 }
>        | formatar '(' expression ',' factor ')'
>                   { E_Formatar $3 $5 }
> {

 Aqui é feito o tratamento de erro gramatical. Caso os analisadores léxico e sintático
 detectem algum tipo de erro, uma mensagm será exibida, informando a linha onde ocorreu
 o erro e abortando o programa em seguida. 

> happyError :: Parse 
> happyError = \s i -> error (
>            "Erro sintatico na linha " ++ show (i::Integer) ++ "\n")

 Abaixo é apresentada a estrutura dos tokens existentes no programa. 

> data Token
>       = TK_INT 
>       | TK_NUM Integer
>       | TK_ATRIB
>       | TK_ID String
>       | TK_STR String
>       | TK_STRING
>       | TK_REAL
>       | TK_FLOAT Double
>       | TK_BOOL
>       | TK_BOOLEAN Bool
>       | TK_PROG
>       | TK_ENDPROG
>       | TK_PV
>       | TK_MAIS
>       | TK_MENOS
>       | TK_PROD
>       | TK_ACOL
>       | TK_FCOL
>       | TK_DIV
>       | TK_IDIV
>       | TK_VIRG
>       | TK_ABREPAR
>       | TK_FECHAPAR
>       | TK_ABRECHAV
>       | TK_FECHACHAV
>       | TK_LEIA
>       | TK_IMPRIMA
>       | TK_IGUAL
>       | TK_MAIOR
>       | TK_MENOR
>       | TK_MAIORIG
>       | TK_MENORIG
>       | TK_DIF
>       | TK_E
>       | TK_OU
>       | TK_NAO
>       | TK_SE
>       | TK_SENAO
>       | TK_ENQUANTO
>       | TK_FACA
>       | TK_EOF
>       | TK_PARA
>       | TK_INCR
>       | TK_DECR
>       | TK_STRCOMP
>       | TK_STRCONCAT
>       | TK_STRCOMPARA
>       | TK_STRNPRIM
>       | TK_STRNRESTO
>       | TK_STRULT
>       | TK_STRCOPIA
>       | TK_STRPRIM
>       | TK_STRCALDA
>       | TK_RAIZQUAD
>       | TK_STRELEM
>       | TK_SENO
>       | TK_COSENO
>       | TK_TANGENTE
>       | TK_RESTO
>       | TK_POT
>       | TK_IPOT
>       | TK_EXP
>       | TK_LOG
>       | TK_PI
>       | TK_ABS
>       | TK_INTREAL
>       | TK_REALINT
>       | TK_SAIA
>       | TK_CONTINUE
>       | TK_PARE
>       | TK_FORMATAR

> lexer :: (Token -> Parse) -> Parse
> lexer cont []  = cont TK_EOF []
> lexer cont (c:cs)
>       | c == '\n' = \line -> lexer cont cs (line+1)
>       | isSpace c = lexer cont cs 
>       | isAlpha c = lexVar cont (c:cs)
>       | isDigit c = lexNum cont (c:cs)

> lexer cont ('"': cs)    
>    = cont (TK_STR str) cs'
>      where
>          (str,cs') = getString cs []
>          getString [] l = error "String incompleta. Insira \".\n"
>          getString ('"' : cs) l = (l,cs)
>          getString ('\\' : 'n': cs) l = getString cs (l++['\n'])
>          getString ('\\' : 't': cs) l = getString cs (l++['\t'])
>          getString (c:cs) l     = getString cs (l++[c])

> lexer cont ('/':'*':cs)    
>     = if cs' /= [] 
>           then \line -> lexer cont cs' (line+l) 
>           else error "Comentário incompleto. Insira */.\n"
>       where
>           (cs',l)                   = getComent cs 0
>           getComent [] l            = ([],l)
>           getComent ('*':'/':cs) l  = (cs,l)
>           getComent ('\n':cs) l     = getComent cs (l+1)
>           getComent (c:cs) l        = getComent cs l

> lexer cont ('#':cs) = \line -> lexer cont cs' (line+1)
>                           where (c:cs') = dropWhile (/= '\n') cs
> lexer cont ('<':'-':cs) = cont TK_ATRIB cs
> lexer cont ('-':'-':cs) = cont TK_DECR cs
> lexer cont ('>':'=':cs) = cont TK_MAIORIG cs
> lexer cont ('<':'=':cs) = cont TK_MENORIG cs
> lexer cont ('<':'>':cs) = cont TK_DIF cs
> lexer cont ('=':'=':cs) = cont TK_IGUAL cs
> lexer cont ('&':'&':cs) = cont TK_E cs
> lexer cont ('|':'|':cs) = cont TK_OU cs
> lexer cont ('+':'+':cs) = cont TK_INCR cs
> lexer cont ('*':'*':cs) = cont TK_POT cs
> lexer cont ('^':cs) = cont TK_IPOT cs
> lexer cont ('+':cs) = cont TK_MAIS cs
> lexer cont ('-':cs) = cont TK_MENOS cs
> lexer cont ('*':cs) = cont TK_PROD cs
> lexer cont ('/':cs) = cont TK_DIV cs
> lexer cont ('[':cs) = cont TK_ACOL cs
> lexer cont (']':cs) = cont TK_FCOL cs
> lexer cont (',':cs) = cont TK_VIRG cs
> lexer cont (';':cs) = cont TK_PV cs
> lexer cont ('(':cs) = cont TK_ABREPAR cs
> lexer cont (')':cs) = cont TK_FECHAPAR cs
> lexer cont ('{':cs) = cont TK_ABRECHAV cs
> lexer cont ('}':cs) = cont TK_FECHACHAV cs
> lexer cont ('>':cs) = cont TK_MAIOR cs
> lexer cont ('<':cs) = cont TK_MENOR cs
> lexer cont ('!':cs) = cont TK_NAO cs
> lexer cont ('%':cs) = cont TK_RESTO cs
> lexer cont ('.':cs) = error "Utilizacao impropria do caracter \".\" "
> lexer cont ('\\':cs) = error "Utilizacao impropria do caracter \"\\\""
> lexer cont ('~':cs) = error "Utilizacao impropria do caracter \"~\""
> lexer cont ('`':cs) = error "Utilizacao impropria do caracter \"`\""
> lexer cont ('@':cs) = error "Utilizacao impropria do caracter \"@\""
> lexer cont ('$':cs) = error "Utilizacao impropria do caracter \"$\""
> lexer cont ('&':cs) = error "Utilizacao impropria do caracter \"&\""
> lexer cont ('_':cs) = error "Utilizacao impropria do caracter \"_\""
> lexer cont ('=':cs) = error "Utilizacao impropria do caracter \"=\""
> lexer cont ('|':cs) = error "Utilizacao impropria do caracter \"|\""
> lexer cont (':':cs) = error "Utilizacao impropria do caracter \":\""
> lexer cont (''':cs) = error "Utilizacao impropria do caracter \"'\""
> lexer cont ('?':cs) = error "Utilizacao impropria do caracter \"?\""

> lexNum cont cs | head rest /= '.' = cont (TK_NUM (read num)) rest
>                | otherwise = cont (TK_FLOAT (read (num ++ ['.'] ++ num'))) rest'
>             where (num,rest) = span isDigit cs
>                   (num',rest') = span isDigit (tail rest)

> lexVar cont cs =
>    case span isAlphaNum cs of
>       ("prog",rest) -> cont TK_PROG rest
>       ("int",rest)  -> cont TK_INT rest
>       ("real",rest) -> cont TK_REAL rest
>       ("logico",rest) -> cont TK_BOOL rest
>       ("string",rest) -> cont TK_STRING rest
>       ("fimprog",rest) -> cont TK_ENDPROG rest
>       ("se",rest) -> cont TK_SE rest
>       ("senao",rest) -> cont TK_SENAO rest
>       ("enquanto",rest) -> cont TK_ENQUANTO rest
>       ("faca",rest) -> cont TK_FACA rest
>       ("para",rest) -> cont TK_PARA rest
>       ("leia",rest) -> cont TK_LEIA rest
>       ("imprima",rest) -> cont TK_IMPRIMA rest
>       ("verdadeiro",rest) -> cont (TK_BOOLEAN True) rest
>       ("falso",rest) -> cont (TK_BOOLEAN False) rest
>       ("strtam",rest) -> cont TK_STRCOMP rest 
>       ("strconcat",rest) -> cont TK_STRCONCAT rest 
>       ("strcomp",rest) -> cont TK_STRCOMPARA rest 
>       ("strult",rest) -> cont TK_STRULT rest 
>       ("strnprim",rest) -> cont TK_STRNPRIM rest 
>       ("strnresto",rest) -> cont TK_STRNRESTO rest 
>       ("strcopia",rest) -> cont TK_STRCOPIA rest 
>       ("strprim",rest) -> cont TK_STRPRIM rest 
>       ("strresto",rest) -> cont TK_STRCALDA rest 
>       ("strelem",rest) -> cont TK_STRELEM rest 
>       ("raiz",rest) -> cont TK_RAIZQUAD rest 
>       ("sen",rest) -> cont TK_SENO rest 
>       ("cos",rest) -> cont TK_COSENO rest 
>       ("tan",rest) -> cont TK_TANGENTE rest 
>       ("exp",rest) -> cont TK_EXP rest 
>       ("log",rest) -> cont TK_LOG rest 
>       ("pi",rest) -> cont TK_PI rest 
>       ("abs",rest) -> cont TK_ABS rest 
>       ("formatar",rest) -> cont TK_FORMATAR rest 
>       ("intreal",rest) -> cont TK_INTREAL rest 
>       ("realint",rest) -> cont TK_REALINT rest 
>       ("saia",rest) -> cont TK_SAIA rest 
>       ("continue",rest) -> cont TK_CONTINUE rest 
>       ("pare",rest) -> cont TK_PARE rest 
>       ("div",rest) -> cont TK_IDIV rest 
>       (var,rest) -> cont (TK_ID var) rest

 Abaixo, é definida a estrutura do monad a ser utilizada pelo parser.

> type P a = String -> Integer -> ParseResult a
> type Parse = P Program
> grammar :: Parse 
> }
