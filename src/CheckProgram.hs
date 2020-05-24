 module CheckProgram (checkProgram,getType, isSimple, isVector, elemVar,
 checkExpType, getVarType, removeVar, checkMemType, fromMtoT, equalType,
 fromExpExp)
 where
 import UalTypes

 checkProgram (Program v ts cs) 
    = do 
          a <- checkDupVar (removeVar ts)
          ts' <- checkCmdType cs ts 
	  return ts

 isSimpleType T_INT = True
 isSimpleType T_REAL = True
 isSimpleType T_STRING = True
 isSimpleType T_BOOL = True
 isSimpleType _ = False

 checkCmdType [] ts = return ts

 checkCmdType ((Atrib (Simple v) e) : cs) ts 
    = if isSimpleType tv then
                         if equalType te tv 
                            then do x <- checkCmdType cs ts
	                            return x
                            else do error ("Tipo incorreto em atribuição: " ++ v ++ " <- " ++ fromExpExp e ++ ".")
                       else error ("Variável " ++ v ++ " não declarada.")
           where te = checkExpType e ts
                 tv = getVarType v ts
 
 checkCmdType ((Atrib (Vector v e') e) : cs) ts 
    = if not (isSimpleType tv) 
             then if equalType te' T_INT 
                     then  do if checkVectorAtrib tv te 
                                 then do x <- checkCmdType cs ts
                                         return x
                                 else do error ("Tipo incorreto em atribuição: " ++ v ++ "[" ++ fromExpExp e' ++ "] <- " ++ fromExpExp e ++ ".")
                     else do error ("Índice incorreto na variável " ++ v ++ "[" ++ fromExpExp e' ++ "].")
             else error ("Variável " ++ v ++ "[" ++ fromExpExp e' ++ "] não declarada.") 
         where te = checkExpType e ts
               tv = getVarType v ts
               te' = checkExpType e' ts

 checkCmdType ((E_Incr (Simple v)) : cs) ts 
    = if isSimpleType tv then
                         if equalType tv T_INT 
                            then do x <- checkCmdType cs ts
	                            return x
                            else do error ("Tipo incorreto em incremento da variavel " ++ v ++ ".")
                       else error ("Variável " ++ v ++ " não declarada.")
           where tv = getVarType v ts
 
 checkCmdType ((E_Incr (Vector v e')) : cs) ts 
    = if not (isSimpleType tv) 
             then if equalType te' T_INT 
                     then  do if checkVectorAtrib tv T_INT 
                                 then do x <- checkCmdType cs ts
                                         return x
                                 else do error ("Tipo incorreto em incremento da variavel " ++ v ++ "[" ++ fromExpExp e' ++ "].")
                     else do error ("Índice incorreto na variável " ++ v ++ "[" ++ fromExpExp e' ++ "].")
             else error ("Variável " ++ v ++ "[" ++ fromExpExp e' ++ "] não declarada.") 
         where tv = getVarType v ts
               te' = checkExpType e' ts

 checkCmdType ((E_Decr (Simple v)) : cs) ts 
    = if isSimpleType tv then
                         if equalType tv T_INT 
                            then do x <- checkCmdType cs ts
	                            return x
                            else do error ("Tipo incorreto em decremento da variavel " ++ v ++ ".")
                       else error ("Variável " ++ v ++ " não declarada.")
           where tv = getVarType v ts
 
 checkCmdType ((E_Decr (Vector v e')) : cs) ts 
    = if not (isSimpleType tv) 
             then if equalType te' T_INT 
                     then  do if checkVectorAtrib tv T_INT 
                                 then do x <- checkCmdType cs ts
                                         return x
                                 else do error ("Tipo incorreto em decremento da variavel " ++ v ++ "[" ++ fromExpExp e' ++ "].")
                     else do error ("Índice incorreto na variável " ++ v ++ "[" ++ fromExpExp e' ++ "].")
             else error ("Variável " ++ v ++ "[" ++ fromExpExp e' ++ "] não declarada.") 
         where tv = getVarType v ts
               te' = checkExpType e' ts

 checkCmdType ((E_Leia (Factor (E_Var v))) : cs) ts
    = if (isSimpleType (getVarType v ts)) 
            then if equalType tv T_BOOL then do error ("Tipo não permitido na leitura da variável " ++ v ++ ".")
                                        else do x <- checkCmdType cs ts
                                                return x
            else error ("Variável " ++ v ++ " não declarada.")
         where tv = getVarType v ts
      
 checkCmdType ((E_Leia (Factor (E_Vector v e))) : cs) ts 
    = if not (isSimpleType (getVarType v ts)) 
            then if (equalType T_INT (checkExpType e ts)) 
                     then if equalType tv T_BOOLVECTOR 
	                     then do error ("Tipo não permitido na leitura da variável " ++ v ++ "[" ++ fromExpExp e ++ "].")
		             else do x <- checkCmdType cs ts
		                     return x
                     else do error ("Indice incorreto na leitura do vetor " ++ v ++ "[" ++ fromExpExp e ++ "].")
            else error ("Variável " ++ v ++ "[" ++ fromExpExp e ++ "] não declarada.") 
        where tv = getVarType v ts
            
 checkCmdType ((E_Imprima []) : cs) ts = checkCmdType cs ts
 checkCmdType ((E_Imprima (e:es)) : cs) ts
      = if f (checkExpType e ts) then do x <- checkCmdType ((E_Imprima es) : cs) ts
                                         return x
                                 else do error ("Tipo não permitido em impressão: " ++ fromExpExp e ++ ".")
      where f T_INT = True
            f T_STRING = True
	    f T_REAL = True
	    f _ = False
	    
 checkCmdType ((E_Se e c) : cs) ts 
    = if equalType te T_BOOL 
          then do x <- checkCmdType c ts
	          y <- checkCmdType cs x
	          return y
          else do error ("Tipo incorreto na condição do se: " ++ fromExpExp e ++ ".")
      where te = checkExpType e ts

 checkCmdType ((E_Senao e c1 c2) : cs) ts 
    = if equalType te T_BOOL 
         then do x <- checkCmdType c1 ts
	         y <- checkCmdType c2 x 
		 z <- checkCmdType cs y
	         return z
         else do error ("Tipo incorreto na condição do senao: " ++ fromExpExp e ++ ".")
      where te = checkExpType e ts

 checkCmdType ((E_Enquanto e c) : cs) ts  
    = if equalType te T_BOOL 
         then do x <- checkCmdType c ts
	         y <- checkCmdType cs x
	         return y
         else do error ("Tipo incorreto na condição do enquanto: " ++ fromExpExp e ++ ".")
      where te = checkExpType e ts

 checkCmdType ((E_Faca c e) : cs) ts  
    = if equalType te T_BOOL 
         then do x <- checkCmdType c ts
	         y <- checkCmdType cs x
	         return y
         else do error ("Tipo incorreto na condição do faca: " ++ fromExpExp e ++ ".")
      where te = checkExpType e ts


 checkCmdType ((E_Para li lf inc c) : cs) ts  
    = if equalType tlf T_BOOL
              then do v <- checkAtribType [li] ts
	              tli <- checkCmdType [li] ts
                      w <- checkForVar [li] [inc]
		      tinc <- checkCmdType [inc] ts
	              x <- checkCmdType c ts
                      y <- checkCmdType cs x
                      return y
              else do error ("Tipo incorreto em limite final " ++ fromExpExp lf ++ ".")
      where tlf  = checkExpType lf ts

 checkCmdType ((E_Saia) : cs) ts 
    = do
         v <- checkCmdType cs ts
	 return v

 checkCmdType ((E_Continue) : cs) ts 
    = do
         v <- checkCmdType cs ts
	 return v

 checkCmdType ((E_Pare) : cs) ts 
    = do
         v <- checkCmdType cs ts
	 return v

 checkAtribType ((Atrib (Simple v) _) : _) ts
    = if equalType tv T_INT
         then do 
	         return v
	 else do error ("Tipo nao inteiro da variavel " ++ v ++ " na inicializacao do para.")
      where tv = getVarType v ts

 checkAtribType ((Atrib (Vector v _) _) : _) ts
    = if equalType tv T_INTVECTOR
         then do 
	         return v
	 else do error ("Tipo nao inteiro do vetor " ++ v ++ " na inicializacao do para.")
      where tv = getVarType v ts

 checkAtribType _ ts
    = error ("Implementacao invalida na inicializacao da funcao para.")
 
 checkForVar ((Atrib (Simple vli) _) : _) ((Atrib (Simple vinc) _) : _) 
        | vli/=vinc 
                = error ("Variaveis "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli

 checkForVar ((Atrib (Simple vli) _) : _) ((E_Incr (Simple vinc)) : _) 
        | vli/=vinc 
                = error ("Variaveis "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli
			
 checkForVar ((Atrib (Simple vli) _) : _) ((E_Decr (Simple vinc)) : _) 
        | vli/=vinc 
                = error ("Variaveis "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli
			
 checkForVar ((Atrib (Vector vli _) _) : _) ((Atrib (Vector vinc _) _) : _) 
        | vli/=vinc 
                = error ("Vetores "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli

 checkForVar ((Atrib (Vector vli _) _) : _) ((E_Incr (Vector vinc _) ) : _) 
        | vli/=vinc 
                = error ("Vetores "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli

 checkForVar ((Atrib (Vector vli _) _) : _) ((E_Decr (Vector vinc _)) : _) 
        | vli/=vinc 
                = error ("Vetores "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli

 checkForVar ((Atrib (Simple vli) _) : _) ((Atrib (Vector vinc _) _) : _) 
        | vli/=vinc 
                = error ("Variaveis "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli

 checkForVar ((Atrib (Simple vli) _) : _) ((E_Incr (Vector vinc _)) : _) 
        | vli/=vinc 
                = error ("Variaveis "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli
 
 checkForVar ((Atrib (Simple vli) _) : _) ((E_Decr (Vector vinc _)) : _) 
        | vli/=vinc 
                = error ("Variaveis "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli
 
 checkForVar ((Atrib (Vector vli _) _) : _) ((Atrib (Simple vinc ) _) : _) 
        | vli/=vinc 
                = error ("Variaveis "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli

 checkForVar ((Atrib (Vector vli _) _) : _) ((E_Incr (Simple vinc )) : _) 
        | vli/=vinc 
                = error ("Variaveis "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli
 
 checkForVar ((Atrib (Vector vli _) _) : _) ((E_Decr (Simple vinc )) : _) 
        | vli/=vinc 
                = error ("Variaveis "  ++ vinc ++ " e " ++ vli ++ " diferentes no comando para.")
	| otherwise = return vli
 
 equalType T_INT T_INT                   = True
 equalType T_REAL T_REAL                 = True
 equalType T_BOOL T_BOOL                 = True
 equalType T_STRING T_STRING             = True
 equalType T_INTVECTOR T_INTVECTOR       = True
 equalType T_REALVECTOR T_REALVECTOR     = True
 equalType T_BOOLVECTOR T_BOOLVECTOR     = True
 equalType T_STRINGVECTOR T_STRINGVECTOR = True
 equalType T_WRONG  T_WRONG              = True
 equalType _ _                           = False

 checkVectorAtrib T_INTVECTOR T_INT       = True
 checkVectorAtrib T_REALVECTOR T_REAL     = True
 checkVectorAtrib T_BOOLVECTOR T_BOOL     = True
 checkVectorAtrib T_STRINGVECTOR T_STRING = True
 checkVectorAtrib T_WRONG  T_WRONG        = True
 checkVectorAtrib _ _                     = False
 
 fromMtoT (M_Int n) = E_Int n
 fromMtoT (M_Float n) = E_Float n
 fromMtoT (M_Boolean n) = E_Boolean n
 fromMtoT (M_Str s) = E_Str s 
 
 checkMemType (M_Int n) = T_INT
 checkMemType (M_Float n) = T_REAL
 checkMemType (M_Boolean n) = T_BOOL
 checkMemType (M_Str s) = T_STRING
 checkMemType M_Garbage = T_GARBAGE

 checkExpType (Factor f) ts        = checkFactorType f ts
 checkExpType (E_Mais e1 e2) ts    = if equalType t T_WRONG 
                                        then error ("Tipo incorreto em adição: " ++ fromExpExp e1 ++ " + " ++
                                                      fromExpExp e2 ++ ".")
					else t
				     where t = typePlus (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_Menos e1 e2) ts   = if equalType t T_WRONG 
                                         then error ("Tipo incorreto em subtração: " ++ fromExpExp e1 ++ 					              " - " ++ fromExpExp e2 ++ ".")
					 else t
			             where t = typeMinus (checkExpType e1 ts) (checkExpType e2 ts)
 
 checkExpType (E_Prod e1 e2) ts    = if equalType t T_WRONG
                                          then error ("Tipo incorreto em multiplicação: " ++ fromExpExp e1
					               ++ " * " ++ fromExpExp e2 ++ ".")
					  else t
			             where t = typeTimes (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_Pot e1 e2) ts     = if equalType t T_WRONG
                                          then error ("Tipo incorreto em potenciacao: " ++ fromExpExp e1 ++
					  " ** " ++ fromExpFactor e2 ++ ".")
					  else t
			             where t = typePot (checkExpType e1 ts) (checkFactorType e2 ts)
 checkExpType (E_IPot e1 e2) ts     = if equalType t T_WRONG
                                          then error ("Tipo incorreto em potenciacao: " ++ fromExpExp e1 ++
					  " ^ " ++ fromExpFactor e2 ++ ".")
					  else t
			             where t = typeIPot (checkExpType e1 ts) (checkFactorType e2 ts)
 checkExpType (E_Div e1 e2) ts     = if equalType t T_WRONG
                                         then error ("Tipo incorreto em divisão: " ++ fromExpExp e1 ++ 
					               " / " ++ fromExpExp e2 ++ ".")
					 else t
				     where t = typeDiv (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_IDiv e1 e2) ts     = if equalType t T_WRONG
                                         then error ("Tipo incorreto em divisão: " ++ fromExpExp e1 ++ 
					               " div " ++ fromExpExp e2 ++ ".")
					 else t
				     where t = typeIDiv (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_Resto e1 e2) ts   = if equalType t T_WRONG
                                         then error ("Tipo incorreto em modulo: " ++ fromExpExp e1 ++ 
					               " / " ++ fromExpExp (Factor e2) ++ ".")
					 else T_INT
				     where t = typeResto (checkExpType e1 ts) (checkFactorType e2 ts)
 checkExpType (E_Igual e1 e2) ts   = if equalType t T_WRONG 
                                          then error ("Tipo incorreto em comparação: " ++ fromExpExp e1 ++ 
					              " = " ++ fromExpExp e2 ++ ".")
					  else t
				     where t = typeEq (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_Maior e1 e2) ts   = if equalType t T_WRONG 
                                         then error ("Tipo incorreto em comparação: " ++ fromExpExp e1 ++ " > "
					             ++ fromExpExp e2 ++ ".")
					 else t
                                     where t = typeGt (checkExpType e1 ts) (checkExpType e2 ts) 
 checkExpType (E_Menor e1 e2) ts   = if equalType t T_WRONG 
                                         then error ("Tipo incorreto em comparação: " ++ fromExpExp e1 ++ " < "
					              ++ fromExpExp e2 ++ ".") 
					 else t
				     where t = typeLt (checkExpType e1 ts) (checkExpType e2 ts) 
 checkExpType (E_Maiorig e1 e2) ts = if equalType t T_WRONG 
                                         then error ("Tipo incorreto em comparação: " ++ fromExpExp e1 ++ " >= "
					              ++ fromExpExp e2 ++ ".")
					 else t
				     where t = typeGeq (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_Menorig e1 e2) ts = if equalType t T_WRONG 
                                         then error ("Tipo incorreto em comparação: " ++ fromExpExp e1 ++ " <= "
					             ++ fromExpExp e2 ++ ".")
					 else t
				     where t = typeLeq (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_Dif e1 e2) ts     = if equalType t T_WRONG 
                                         then error ("Tipo incorreto em comparação: " ++ fromExpExp e1 ++ " <> "
					              ++ fromExpExp e2 ++ ".")
					 else t
				     where t = typeNeq (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_E e1 e2) ts       = if equalType t T_WRONG 
                                         then error ("Tipo incorreto em conjunção: " ++ fromExpExp e1 ++ " && "
					              ++ fromExpExp e2 ++ ".")
					 else t
				     where t = typeAnd (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_Ou e1 e2) ts      = if equalType t T_WRONG
                                         then error ("Tipo incorreto em disjunção: " ++ fromExpExp e1 ++ " || "
					              ++ fromExpExp e2 ++ ".")
					 else t
				     where t = typeOr (checkExpType e1 ts) (checkExpType e2 ts)
 checkExpType (E_Nao e) ts         = if equalType t T_WRONG 
                                         then error ("Tipo incorreto em negação: !" ++ fromExpExp e) 
					 else t
				     where t = typeNot (checkExpType e ts)

 isSimpleVar (Simple v) = True
 isSimpleVar (Vector v d) = False

 getVarType v [] = error ("Variável " ++ v ++ " não declarada!")

 getVarType v ((VarInt l) : ls) 
    = if elemSimpleVector v l 
         then if isSimple tag 
	              then T_INT
                      else if isVector tag then T_INTVECTOR
                                           else getVarType v ls
	 else getVarType v ls
      where tag = getType v l

 getVarType v ((VarReal l) : ls) 
    = if elemSimpleVector v l
         then if isSimple tag then T_REAL
                              else if isVector tag then T_REALVECTOR
                                                   else getVarType v ls
	 else getVarType v ls
      where tag = getType v l

 getVarType v ((VarString l) : ls) 
    = if elemSimpleVector v l
         then if isSimple tag then T_STRING
                              else if isVector tag then T_STRINGVECTOR
                                                   else getVarType v ls
	 else getVarType v ls
      where tag = getType v l

 getVarType v ((VarBool l) : ls) 
    = if elemSimpleVector v l
         then if isSimple tag then T_BOOL
	  	              else if isVector tag then T_BOOLVECTOR
                                                   else getVarType v ls
	 else getVarType v ls
      where tag = getType v l

 elemSimpleVector v [] = False
 elemSimpleVector v ((Simple v'):vs) = v == v' || 
                                        elemSimpleVector v vs
 elemSimpleVector v ((Vector v' _):vs) = v == v' || 
                                          elemSimpleVector v vs
 isVector T_VECTOR = True
 isVector _        = False

 isSimple T_SIMPLE = True
 isSimple _        = False

 getType v [] = error "Erro interno No. 100. Por favor, entre em contato com os desenvolvedores do sistema."
 getType v (v':vs) = if cmp v v' then getTag v' 
                     else getType v vs
                     where cmp v (Vector v' d) = v == v'
                           cmp v (Simple v')   = v == v'

 getTag (Vector v d) = T_VECTOR
 getTag (Simple v) = T_SIMPLE

 checkFactorType (E_Int n) ts   = T_INT
 checkFactorType (E_Float n) ts = T_REAL
 checkFactorType (E_Var v) ts   
     =  if isSimpleType tv then tv
                           else error ("Variável " ++ v ++ " não declarada.")
        where tv = getVarType v ts
 checkFactorType (E_Boolean b) ts  = T_BOOL
 checkFactorType (E_Str s) ts = T_STRING
 checkFactorType (E_Vector v e) ts 
     =  if not (isSimpleType tv)
            then if equalIntIndex indextype T_INT 
                     then conv typ
                     else error ("Índice incorreto no vetor " ++ v ++ ".")
            else error ("Variável " ++ v ++ " não declarada.")
        where typ = getVarType v ts
              indextype = checkExpType e ts
              equalIntIndex T_INT T_INT  = True
	      equalIntIndex _ _          = False
	      conv T_INTVECTOR           = T_INT
	      conv T_BOOLVECTOR          = T_BOOL
	      conv T_REALVECTOR          = T_REAL
	      conv T_STRINGVECTOR        = T_STRING
              tv = getVarType v ts
 checkFactorType (E_Par e) ts   = checkExpType e ts
 checkFactorType (E_Pos e) ts   = checkFactorType e ts 
 checkFactorType (E_Neg e) ts   = checkFactorType e ts
 checkFactorType (E_StrComp s) ts = if equalType t T_STRING
                                       then T_INT
				       else error ("Tipo incorreto na funcao strtam.") 
				    where t = checkFactorType s ts
 checkFactorType (E_StrUlt s) ts = if equalType t T_STRING
                                       then T_STRING
				       else error ("Tipo incorreto na funcao strult.") 
				    where t = checkFactorType s ts
 checkFactorType (E_StrCompara s1 s2) ts = if equalType t1 T_STRING && equalType t2 T_STRING
                                              then T_STRING
				              else error ("Tipo incorreto na funcao strcomp.") 
				            where t1 = checkFactorType s1 ts
					          t2 = checkFactorType s2 ts
 checkFactorType (E_StrConcat s1 s2) ts = if equalType t1 T_STRING
                                             then if equalType t2 T_STRING
						     then t1
				                     else error ("Tipo incorreto na funcao strconcat.") 
				             else error ("Tipo incorreto na funcao strconcat.") 
				          where t1 = checkFactorType s1 ts
				                t2 = checkFactorType s2 ts
 checkFactorType (E_StrCopia s) ts = if equalType t T_STRING
                                        then t
				        else error ("Tipo incorreto na funcao strcopia.") 
				     where t = checkFactorType s ts
 checkFactorType (E_StrPrim s) ts = if equalType t T_STRING
                                       then t
				       else error ("Tipo incorreto na funcao strprim.") 
				    where t = checkFactorType s ts
 checkFactorType (E_StrCalda s) ts = if equalType t T_STRING
                                        then t
				        else error ("Tipo incorreto na funcao strresto.") 
				     where t = checkFactorType s ts
 checkFactorType (E_StrElem s n) ts = if (equalType t T_STRING && equalType t' T_INT)
                                         then t
				         else error ("Tipo incorreto na funcao strelem.") 
				      where t = checkFactorType s ts
				            t' = checkFactorType n ts
 checkFactorType (E_StrNPrim s n) ts = if (equalType t T_STRING && equalType t' T_INT)
                                         then t
				         else error ("Tipo incorreto na funcao strnprim.") 
 				       where t = checkFactorType s ts
				             t' = checkFactorType n ts
 checkFactorType (E_StrNResto s n) ts = if (equalType t T_STRING && equalType t' T_INT)
                                          then t
				          else error ("Tipo incorreto na funcao strnresto.") 
 			 	        where t = checkFactorType s ts
				              t' = checkFactorType n ts
 checkFactorType (E_RaizQuad e) ts = if (equalType t T_REAL || equalType t T_INT)
                                        then T_REAL
				        else error ("Tipo incorreto na funcao raiz.") 
				     where t = checkExpType e ts
 checkFactorType (E_Formatar n1 n2) ts = if (equalType t1 T_REAL && equalType t2 T_INT)
                                            then T_STRING
				            else error ("Tipo incorreto na funcao formatar.")
				         where t1 = checkExpType n1 ts
					       t2 = checkFactorType n2 ts
 checkFactorType (E_Seno e) ts = if (equalType t T_REAL || equalType t T_INT)
                                    then T_REAL
				    else error ("Tipo incorreto na funcao sen.") 
				 where t = checkExpType e ts
 checkFactorType (E_Coseno e) ts = if (equalType t T_REAL || equalType t T_INT)
                                      then T_REAL
				      else error ("Tipo incorreto na funcao cos.") 
				   where t = checkExpType e ts
 checkFactorType (E_Tangente e) ts = if (equalType t T_REAL || equalType t T_INT)
                                        then T_REAL
				        else error ("Tipo incorreto na funcao tan.") 
				     where t = checkExpType e ts
 checkFactorType (E_Exp e) ts = if (equalType t T_REAL || equalType t T_INT)
                                   then T_REAL
				   else error ("Tipo incorreto na funcao exp.") 
				where t = checkExpType e ts
 checkFactorType (E_Log e) ts = if (equalType t T_REAL || equalType t T_INT)
                                   then T_REAL
			           else error ("Tipo incorreto na funcao log.") 
				where t = checkExpType e ts
 checkFactorType (E_Abs e) ts = if (equalType t T_REAL || equalType t T_INT)
                                   then t
			           else error ("Tipo incorreto na funcao abs.") 
				where t = checkExpType e ts
 checkFactorType (E_Pi) ts = T_REAL
 checkFactorType (E_IntReal e) ts = if (equalType t T_INT)
                                       then T_REAL
			               else error ("Tipo incorreto na funcao intreal.") 
				    where t = checkExpType e ts
 checkFactorType (E_RealInt e) ts = if (equalType t T_REAL)
                                       then T_INT
			               else error ("Tipo incorreto na funcao realint.") 
				    where t = checkExpType e ts
 
 typePlus T_INT T_INT                = T_INT 
 typePlus T_INT T_INTVECTOR          = T_INT 
 typePlus T_INTVECTOR T_INT          = T_INT 
 typePlus T_INTVECTOR T_INTVECTOR    = T_INT 
 typePlus T_REAL  T_REAL             = T_REAL 
 typePlus T_REAL  T_REALVECTOR       = T_REAL 
 typePlus T_REALVECTOR  T_REAL       = T_REAL 
 typePlus T_REALVECTOR  T_REALVECTOR = T_REAL 
 typePlus T_INT T_REAL               = T_REAL 
 typePlus T_INT T_REALVECTOR         = T_REAL 
 typePlus T_INTVECTOR T_REAL         = T_REAL 
 typePlus T_INTVECTOR T_REALVECTOR   = T_REAL 
 typePlus T_REAL  T_INT              = T_REAL 
 typePlus T_REAL  T_INTVECTOR        = T_REAL 
 typePlus T_REALVECTOR  T_INT        = T_REAL 
 typePlus T_REALVECTOR  T_INTVECTOR  = T_REAL 
 typePlus _  _                       = T_WRONG

 typeMinus T_INT T_INT                = T_INT 
 typeMinus T_INT T_INTVECTOR          = T_INT 
 typeMinus T_INTVECTOR T_INT          = T_INT 
 typeMinus T_INTVECTOR T_INTVECTOR    = T_INT 
 typeMinus T_REAL  T_REAL             = T_REAL 
 typeMinus T_REAL  T_REALVECTOR       = T_REAL 
 typeMinus T_REALVECTOR  T_REAL       = T_REAL 
 typeMinus T_REALVECTOR  T_REALVECTOR = T_REAL 
 typeMinus T_REAL  T_INT              = T_REAL 
 typeMinus T_REAL  T_INTVECTOR        = T_REAL 
 typeMinus T_REALVECTOR  T_INT        = T_REAL 
 typeMinus T_REALVECTOR  T_INTVECTOR  = T_REAL 
 typeMinus T_INT  T_REAL              = T_REAL 
 typeMinus T_INT  T_REALVECTOR        = T_REAL 
 typeMinus T_INTVECTOR  T_REAL        = T_REAL 
 
 typeTimes T_INT  T_INT               = T_INT 
 typeTimes T_INT T_INTVECTOR          = T_INT 
 typeTimes T_INTVECTOR T_INT          = T_INT 
 typeTimes T_INTVECTOR T_INTVECTOR    = T_INT 
 typeTimes T_REAL  T_REAL             = T_REAL 
 typeTimes T_REAL T_REALVECTOR        = T_REAL 
 typeTimes T_REALVECTOR T_REAL        = T_REAL 
 typeTimes T_REALVECTOR T_REALVECTOR  = T_REAL 
 typeTimes T_INT  T_REAL              = T_REAL 
 typeTimes T_INT T_REALVECTOR         = T_REAL 
 typeTimes T_INTVECTOR T_REAL         = T_REAL 
 typeTimes T_INTVECTOR T_REALVECTOR   = T_REAL 
 typeTimes T_REAL  T_INT              = T_REAL 
 typeTimes T_REAL T_INTVECTOR         = T_REAL 
 typeTimes T_REALVECTOR T_INT         = T_REAL 
 typeTimes T_REALVECTOR T_INTVECTOR   = T_REAL 
 typeTimes _  _                       = T_WRONG 

 typePot T_INT  T_INT                 = T_REAL
 typePot T_INT T_INTVECTOR            = T_REAL
 typePot T_INTVECTOR T_INT            = T_REAL
 typePot T_INTVECTOR T_INTVECTOR      = T_REAL
 typePot T_REAL  T_REAL               = T_REAL 
 typePot T_REAL T_REALVECTOR          = T_REAL 
 typePot T_REALVECTOR T_REAL          = T_REAL 
 typePot T_REALVECTOR T_REALVECTOR    = T_REAL
 typePot T_INT  T_REAL                = T_REAL
 typePot T_INT T_REALVECTOR           = T_REAL
 typePot T_INTVECTOR T_REAL           = T_REAL
 typePot T_INTVECTOR T_REALVECTOR     = T_REAL
 typePot T_REAL  T_INT                = T_REAL 
 typePot T_REAL T_INTVECTOR           = T_REAL 
 typePot T_REALVECTOR T_INT           = T_REAL 
 typePot T_REALVECTOR T_INTVECTOR     = T_REAL 
 typePot _  _                         = T_WRONG 

 typeIPot T_INT  T_INT                 = T_INT
 typeIPot T_INT T_INTVECTOR            = T_INT
 typeIPot T_INTVECTOR T_INT            = T_INT
 typeIPot T_INTVECTOR T_INTVECTOR      = T_INT
 typeIPot T_REAL  T_REAL               = T_INT 
 typeIPot T_REAL T_REALVECTOR          = T_INT 
 typeIPot T_REALVECTOR T_REAL          = T_INT 
 typeIPot T_REALVECTOR T_REALVECTOR    = T_INT
 typeIPot T_INT  T_REAL                = T_INT
 typeIPot T_INT T_REALVECTOR           = T_INT
 typeIPot T_INTVECTOR T_REAL           = T_INT
 typeIPot T_INTVECTOR T_REALVECTOR     = T_INT
 typeIPot T_REAL  T_INT                = T_INT 
 typeIPot T_REAL T_INTVECTOR           = T_INT 
 typeIPot T_REALVECTOR T_INT           = T_INT 
 typeIPot T_REALVECTOR T_INTVECTOR     = T_INT 
 typeIPot _  _                         = T_WRONG 
 
 typeDiv T_INT  T_INT                 = T_REAL
 typeDiv T_INT T_INTVECTOR            = T_REAL 
 typeDiv T_INTVECTOR T_INT            = T_REAL
 typeDiv T_INTVECTOR T_INTVECTOR      = T_REAL
 typeDiv T_REAL  T_REAL               = T_REAL 
 typeDiv T_REAL T_REALVECTOR          = T_REAL
 typeDiv T_REALVECTOR T_REAL          = T_REAL
 typeDiv T_REALVECTOR T_REALVECTOR    = T_REAL
 typeDiv T_INT  T_REAL                = T_REAL 
 typeDiv T_INT T_REALVECTOR           = T_REAL
 typeDiv T_INTVECTOR T_REAL           = T_REAL
 typeDiv T_INTVECTOR T_REALVECTOR     = T_REAL
 typeDiv T_REAL T_INT                 = T_REAL 
 typeDiv T_REAL T_INTVECTOR           = T_REAL
 typeDiv T_REALVECTOR T_INT           = T_REAL
 typeDiv T_REALVECTOR T_INTVECTOR     = T_REAL
 typeDiv _  _                         = T_WRONG


 typeIDiv T_INT  T_INT                 = T_INT
 typeIDiv T_INT T_INTVECTOR            = T_INT 
 typeIDiv T_INTVECTOR T_INT            = T_INT
 typeIDiv T_INTVECTOR T_INTVECTOR      = T_INT
 typeIDiv T_REAL  T_REAL               = T_INT 
 typeIDiv T_REAL T_REALVECTOR          = T_INT
 typeIDiv T_REALVECTOR T_REAL          = T_INT
 typeIDiv T_REALVECTOR T_REALVECTOR    = T_INT
 typeIDiv T_INT  T_REAL                = T_INT 
 typeIDiv T_INT T_REALVECTOR           = T_INT
 typeIDiv T_INTVECTOR T_REAL           = T_INT
 typeIDiv T_INTVECTOR T_REALVECTOR     = T_INT
 typeIDiv T_REAL T_INT                 = T_INT 
 typeIDiv T_REAL T_INTVECTOR           = T_INT
 typeIDiv T_REALVECTOR T_INT           = T_INT
 typeIDiv T_REALVECTOR T_INTVECTOR     = T_INT
 typeIDiv _  _                         = T_WRONG
 
 typeResto T_INT  T_INT                 = T_INT
 typeResto T_INT T_INTVECTOR            = T_INT 
 typeResto T_INTVECTOR T_INT            = T_INT 
 typeResto T_INTVECTOR T_INTVECTOR      = T_INT 
 typeResto _  _                         = T_WRONG
 
 typeEq T_INT T_INT                    = T_BOOL
 typeEq T_INT T_INTVECTOR              = T_BOOL 
 typeEq T_INTVECTOR T_INT              = T_BOOL 
 typeEq T_INTVECTOR T_INTVECTOR        = T_BOOL 
 typeEq T_BOOL T_BOOL                  = T_BOOL
 typeEq T_BOOL T_BOOLVECTOR            = T_BOOL 
 typeEq T_BOOLVECTOR T_BOOL            = T_BOOL 
 typeEq T_BOOLVECTOR T_BOOLVECTOR      = T_BOOL 
 typeEq T_REAL T_REAL                  = T_BOOL
 typeEq T_REAL T_REALVECTOR            = T_BOOL 
 typeEq T_REALVECTOR T_REAL            = T_BOOL 
 typeEq T_REALVECTOR T_REALVECTOR      = T_BOOL 
 typeEq T_STRING T_STRING              = T_BOOL
 typeEq T_STRING T_STRINGVECTOR        = T_BOOL 
 typeEq T_STRINGVECTOR T_STRING        = T_BOOL 
 typeEq T_STRINGVECTOR T_STRINGVECTOR  = T_BOOL 
 typeEq _ _                            = T_WRONG

 typeGt T_INT T_INT                    = T_BOOL
 typeGt T_INT T_INTVECTOR              = T_BOOL 
 typeGt T_INTVECTOR T_INT              = T_BOOL 
 typeGt T_INTVECTOR T_INTVECTOR        = T_BOOL 
 typeGt T_REAL T_REAL                  = T_BOOL
 typeGt T_REAL T_REALVECTOR            = T_BOOL 
 typeGt T_REALVECTOR T_REAL            = T_BOOL 
 typeGt T_REALVECTOR T_REALVECTOR      = T_BOOL 
 typeGt T_STRING T_STRING              = T_BOOL
 typeGt T_STRING T_STRINGVECTOR        = T_BOOL 
 typeGt T_STRINGVECTOR T_STRING        = T_BOOL 
 typeGt T_STRINGVECTOR T_STRINGVECTOR  = T_BOOL 
 typeGt _ _                            = T_WRONG

 typeLt T_INT T_INT                    = T_BOOL
 typeLt T_INT T_INTVECTOR              = T_BOOL 
 typeLt T_INTVECTOR T_INT              = T_BOOL 
 typeLt T_INTVECTOR T_INTVECTOR        = T_BOOL 
 typeLt T_REAL T_REAL                  = T_BOOL
 typeLt T_REAL T_REALVECTOR            = T_BOOL 
 typeLt T_REALVECTOR T_REAL            = T_BOOL 
 typeLt T_REALVECTOR T_REALVECTOR      = T_BOOL 
 typeLt T_STRING T_STRING              = T_BOOL
 typeLt T_STRING T_STRINGVECTOR        = T_BOOL 
 typeLt T_STRINGVECTOR T_STRING        = T_BOOL 
 typeLt T_STRINGVECTOR T_STRINGVECTOR  = T_BOOL 
 typeLt _ _                            = T_WRONG

 typeGeq T_INT T_INT                   = T_BOOL
 typeGeq T_INT T_INTVECTOR             = T_BOOL 
 typeGeq T_INTVECTOR T_INT             = T_BOOL 
 typeGeq T_INTVECTOR T_INTVECTOR       = T_BOOL 
 typeGeq T_REAL T_REAL                 = T_BOOL
 typeGeq T_REALVECTOR T_REAL           = T_BOOL
 typeGeq T_REAL T_REALVECTOR           = T_BOOL 
 typeGeq T_REALVECTOR T_REALVECTOR     = T_BOOL 
 typeGeq T_STRING T_STRING             = T_BOOL
 typeGeq T_STRING T_STRINGVECTOR       = T_BOOL 
 typeGeq T_STRINGVECTOR T_STRING       = T_BOOL 
 typeGeq T_STRINGVECTOR T_STRINGVECTOR = T_BOOL 
 typeGeq _ _                           = T_WRONG

 typeLeq T_INT  T_INT                  = T_BOOL
 typeLeq T_INT T_INTVECTOR             = T_BOOL 
 typeLeq T_INTVECTOR T_INT             = T_BOOL 
 typeLeq T_INTVECTOR T_INTVECTOR       = T_BOOL 
 typeLeq T_REAL T_REAL                 = T_BOOL
 typeLeq T_REALVECTOR T_REAL           = T_BOOL
 typeLeq T_REAL T_REALVECTOR           = T_BOOL 
 typeLeq T_REALVECTOR T_REALVECTOR     = T_BOOL 
 typeLeq T_STRING T_STRING             = T_BOOL
 typeLeq T_STRING T_STRINGVECTOR       = T_BOOL 
 typeLeq T_STRINGVECTOR T_STRING       = T_BOOL 
 typeLeq T_STRINGVECTOR T_STRINGVECTOR = T_BOOL 
 typeLeq _ _                           = T_WRONG

 typeNeq T_INT T_INT                   = T_BOOL
 typeNeq T_INT T_INTVECTOR             = T_BOOL 
 typeNeq T_INTVECTOR T_INT             = T_BOOL 
 typeNeq T_INTVECTOR T_INTVECTOR       = T_BOOL 
 typeNeq T_REAL T_REAL                 = T_BOOL
 typeNeq T_REALVECTOR T_REAL           = T_BOOL
 typeNeq T_REAL T_REALVECTOR           = T_BOOL 
 typeNeq T_REALVECTOR T_REALVECTOR     = T_BOOL 
 typeNeq T_STRING T_STRING             = T_BOOL
 typeNeq T_STRINGVECTOR T_STRING       = T_BOOL
 typeNeq T_STRING T_STRINGVECTOR       = T_BOOL 
 typeNeq T_STRINGVECTOR T_STRINGVECTOR = T_BOOL 
 typeNeq _ _                           = T_WRONG

 typeAnd T_BOOL T_BOOL             = T_BOOL
 typeAnd T_BOOL T_BOOLVECTOR       = T_BOOL
 typeAnd T_BOOLVECTOR T_BOOL       = T_BOOL
 typeAnd T_BOOLVECTOR T_BOOLVECTOR = T_BOOL 
 typeAnd _ _                       = T_WRONG

 typeOr T_BOOL T_BOOL             = T_BOOL
 typeOr T_BOOL T_BOOLVECTOR       = T_BOOL
 typeOr T_BOOLVECTOR T_BOOL       = T_BOOL
 typeOr T_BOOLVECTOR T_BOOLVECTOR = T_BOOL 
 typeOr _ _                       = T_WRONG

 typeNot T_BOOL              = T_BOOL
 typeNot T_BOOLVECTOR        = T_BOOL
 typeNot _                         = T_WRONG

 removeVar [] = []
 removeVar (VarInt l : ls)    = l ++ removeVar ls
 removeVar (VarReal l : ls)   = l ++ removeVar ls
 removeVar (VarBool l : ls)   = l ++ removeVar ls
 removeVar (VarString l : ls) = l ++ removeVar ls

 checkDupVar [] = return []
 checkDupVar (t : ts)  | elemVar t ts = error ("Variável " ++ getVarName t ++ " redeclarada.")
                       | otherwise = do
		                        x <- checkDupVar ts
					return x
                                     where getVarName (Simple v) = v
			                   getVarName (Vector v _) = v
		         
 elemVar v [] = False
 elemVar v (v':vs) = cmp v v' || elemVar v vs
                     where cmp (Simple v) (Vector v' d)    = v == v'
                           cmp (Simple v) (Simple v')      = v == v'
		           cmp (Vector v d) (Simple v')    = v == v'
		           cmp (Vector v d) (Vector v' d') = v == v'

 fromExpExp (Factor f) = fromExpFactor f 
 fromExpExp (E_Mais e1 e2) = fromExpExp e1 ++ "+" ++ fromExpExp e2 

 fromExpExp (E_Menos e1 e2) = fromExpExp e1 ++ "-" ++ fromExpExp e2
 fromExpExp (E_Prod e1 e2) = fromExpExp e1 ++ "*" ++ fromExpExp e2 
 fromExpExp (E_Pot e1 e2) = fromExpExp e1 ++ "**" ++ fromExpFactor e2 
 fromExpExp (E_Div e1 e2) = fromExpExp e1 ++ "/" ++ fromExpExp e2 
 fromExpExp (E_Resto e1 e2) = fromExpExp e1 ++ "%" ++ fromExpFactor e2 
 fromExpExp (E_Igual e1 e2) = fromExpExp e1 ++ "=" ++ fromExpExp e2 
 fromExpExp (E_Maior e1 e2) = fromExpExp e1 ++ ">" ++ fromExpExp e2
 fromExpExp (E_Menor e1 e2) = fromExpExp e1 ++ "<" ++ fromExpExp e2
 fromExpExp (E_Maiorig e1 e2) = fromExpExp e1 ++ ">=" ++ fromExpExp e2 
 fromExpExp (E_Menorig e1 e2) = fromExpExp e1 ++ "<=" ++ fromExpExp e2 
 fromExpExp (E_Dif e1 e2) = fromExpExp e1 ++ "<>" ++ fromExpExp e2 
 fromExpExp (E_E e1 e2) = fromExpExp e1 ++ "&&" ++ fromExpExp e2 
 fromExpExp (E_Ou e1 e2) = fromExpExp e1 ++ "||" ++ fromExpExp e2 
 fromExpExp (E_Nao e) = "!" ++ fromExpExp e

 fromExpFactor (E_Int n) = show n
 fromExpFactor (E_Float n) = show n
 fromExpFactor (E_Var v) = v
 fromExpFactor (E_Boolean b) = show b 
 fromExpFactor (E_Vector v e) = v ++ "[" ++ fromExpExp e ++ "]"
 fromExpFactor (E_Str s) = s
 fromExpFactor (E_Par e) = "(" ++ fromExpExp e ++ ")"
 fromExpFactor (E_Pos n) = fromExpFactor n 
 fromExpFactor (E_Neg n) = fromExpFactor n
 fromExpFactor (E_StrComp s) = "strtam(" ++ fromExpFactor s ++ ")"
 fromExpFactor (E_StrConcat s1 s2) = "strconcat(" ++ fromExpFactor s1 ++ "," ++ fromExpFactor s2 ++ ")"
 fromExpFactor (E_StrCopia s) = "strcopia(" ++ fromExpFactor s ++ ")"
 fromExpFactor (E_StrCompara s1 s2) = "strcompara(" ++ fromExpFactor s1 ++ "," ++ fromExpFactor s2 ++ ")"
 fromExpFactor (E_StrUlt s) = "strult(" ++ fromExpFactor s ++ ")"
 fromExpFactor (E_StrNPrim s n) = "strnprim(" ++ fromExpFactor s ++ "," ++ fromExpFactor n ++ ")"
 fromExpFactor (E_StrNResto s n) = "strnresto(" ++ fromExpFactor s ++ "," ++ fromExpFactor n ++ ") "
 fromExpFactor (E_StrPrim s) = "strprim(" ++ fromExpFactor s ++ ")"
 fromExpFactor (E_StrCalda s) = "strresto(" ++ fromExpFactor s ++ ")"
 fromExpFactor (E_StrElem s n) = "strelem(" ++ fromExpFactor s ++ "," ++ fromExpFactor n ++ ")"
 fromExpFactor (E_Formatar n1 n2) = "formatar(" ++ fromExpExp n1 ++ "," ++ fromExpFactor n2 ++ ")"
 fromExpFactor (E_RaizQuad e) = "raiz(" ++ fromExpExp e ++ ")"
 fromExpFactor (E_Seno e) = "sen(" ++ fromExpExp e ++ ")"
 fromExpFactor (E_Coseno e) = "cos(" ++ fromExpExp e ++ ")"
 fromExpFactor (E_Tangente e) = "tan(" ++ fromExpExp e ++ ")"
 fromExpFactor (E_Exp e) = "exp(" ++ fromExpExp e ++ ")"
 fromExpFactor (E_Log e) = "log(" ++ fromExpExp e ++ ")"
 fromExpFactor (E_Abs e) = "abs(" ++ fromExpExp e ++ ")"
 fromExpFactor (E_Pi) = "pi"
 fromExpFactor (E_IntReal e) = "intreal(" ++ fromExpExp e ++ ")"
 fromExpFactor (E_RealInt e) = "realint(" ++ fromExpExp e ++ ")"
