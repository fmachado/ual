 module UalTypes 
 (Program (..), TypeGram (..), 
 Comand (..), Exp (..), Factor (..), TypeCheck (..), Var (..) , Memory (..)
 ) 

 where

 data Program = Program Var [TypeGram] [Comand]

 data TypeGram = VarInt    [Var]
               | VarReal   [Var]
               | VarBool   [Var]
               | VarString [Var]

 data Var = Simple String
          | Vector String Exp

 data Comand = Atrib Var Exp
             | E_Se Exp [Comand]
             | E_Senao Exp [Comand] [Comand]
             | E_Enquanto Exp [Comand]
             | E_Faca [Comand] Exp
             | E_Para Comand Exp Comand [Comand]
             | E_Leia Exp
             | E_Imprima [Exp]
	     | E_Incr Var
	     | E_Decr Var
	     | E_AtribFor Var Exp
	     | E_IncrFor Var
	     | E_DecrFor Var
	     | E_Saia
	     | E_Continue
	     | E_Pare
             | Ann_Atrib Var Exp
             | Ann_Se Exp [(String,String,Comand)]
             | Ann_Senao Exp [(String,String,Comand)] [(String,String,Comand)]
             | Ann_Enquanto Exp [(String,String,Comand)]
             | Ann_Faca [(String,String,Comand)] Exp
             | Ann_Para [(String,String,Comand)] Exp Comand [(String,String,Comand)]
             | Ann_Imprima [Exp]
             | Ann_Leia Exp
	     | Ann_Incr Var
	     | Ann_Decr Var
	     | Ann_AtribFor Var Exp
	     | Ann_IncrFor Var
	     | Ann_DecrFor Var
	     | Ann_Saia
	     | Ann_Continue
	     | Ann_Pare

 data Exp = Factor Factor
          | E_Mais Exp Exp
          | E_Menos Exp Exp
          | E_Prod Exp Exp
          | E_Div Exp Exp
          | E_IDiv Exp Exp
          | E_Igual Exp Exp
          | E_Maior Exp Exp
          | E_Menor Exp Exp 
          | E_Maiorig Exp Exp
          | E_Menorig Exp Exp
          | E_Dif Exp Exp
          | E_E Exp Exp
          | E_Ou Exp Exp
          | E_Nao Exp
	  | E_Resto Exp Factor
	  | E_Pot Exp Factor
	  | E_IPot Exp Factor

 data Factor = E_Int Integer
             | E_Boolean Bool
             | E_Var String
	     | E_Vector String Exp
             | E_Float Double
             | E_Str String
             | E_Par Exp
	     | E_Pos Factor
	     | E_Neg Factor
	     | E_StrComp Factor
	     | E_StrConcat Factor Factor
	     | E_StrCompara Factor Factor
	     | E_StrNPrim Factor Factor
	     | E_StrNResto Factor Factor
	     | E_StrUlt Factor
	     | E_StrCopia Factor
	     | E_StrPrim Factor
	     | E_StrCalda Factor
	     | E_StrElem Factor Factor
	     | E_RaizQuad Exp
	     | E_Seno Exp
	     | E_Coseno Exp
	     | E_Tangente Exp
	     | E_Exp Exp
	     | E_Log Exp
	     | E_Pi
	     | E_Abs Exp
	     | E_RealInt Exp
	     | E_IntReal Exp
	     | E_Formatar Exp Factor

 data Memory = M_Int Integer
             | M_Boolean Bool
             | M_Var String
	     | M_Vector [Memory]
             | M_Float Double
             | M_Str String
	     | M_Garbage 
	     
 data TypeCheck = T_INT
                | T_BOOL
                | T_REAL
                | T_STRING
                | T_INTVECTOR
                | T_BOOLVECTOR
                | T_REALVECTOR
                | T_STRINGVECTOR
                | T_SIMPLE
                | T_VECTOR
		| T_GARBAGE
		| T_WRONG
