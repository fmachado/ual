 module EvalExp (evalExp,evalFactor,seekVarValue,   
 seekVectorValue,evalUpdate,evalVectorUpdate,
 evalPlus,evalMinus,evalTimes,evalDiv,evalEq, 
 evalGt,evalLt,evalGeq,evalLeq,evalNeq,evalAnd,evalOr,
 fillGarbage,evalNot,getReadInt,getReadIntVector,
 getReadFloat,getReadFloatVector,fromMem, evalIncrVar, 
 evalIncrVector,evalDecrVar, evalDecrVector)
 where
 import UalTypes
 import PrelNum 
 import IO
 import Grammar 
 import ParseTypes
 import CheckProgram
 import Char ( isDigit, isAlpha)

 getReadInt (x:xs) v
   = do
       if x == '-'
	  then if rest /= ""
                  then getMsg v
		  else return (M_Int (-1 * (read xs)))
	  else if x == '+'
		  then if rest /= "" 
		          then getMsg v
			  else return (M_Int (read xs))
		  else if rest' /= ""
		          then getMsg v
			  else return (M_Int (read (x:xs)))
       where (num,rest) = span isDigit xs
	     (num',rest') = span isDigit (x:xs) 

 getMsg v 
   = do
       error ("Tipo incorreto na leitura da variável " ++ v ++ ".")

 getReadIntVector (x:xs) v i
   = do
       if x == '-'
          then if rest /= ""
                  then getMsg2 v i
                  else return (M_Int (-1 * (read xs)))
          else if x == '+'
                  then if rest /= ""
                          then getMsg2 v i
                          else return (M_Int (read xs))
                  else if rest' /= ""
                          then getMsg2 v i
                          else return (M_Int (read (x:xs)))
       where (num,rest) = span isDigit xs
	     (num',rest') = span isDigit (x:xs) 

 getMsg2 v i
   = do
       error ("Tipo incorreto na leitura da variável " ++ v ++ "[" ++ fromExpExp i ++ "].")

 getReadFloat (x:xs) v
    = do
        if x == '-'
	   then if rest /= ""
                   then if head rest == '.'
                           then if rest' /= ""
                                   then getMsg v
                                   else return (M_Float (-1.0 * (read xs)))
                           else getMsg v
                   else getMsg v
           else if x == '+'
	           then if rest /= ""
                           then if head rest == '.'
                                   then if rest' /= ""
                                           then getMsg v
                                           else return (M_Float (read xs))
                                   else getMsg v
                           else getMsg v
                   else if rest'' /= ""
                           then if head rest'' == '.'
                                   then if rest''' /= ""
                                           then getMsg v
                                           else return (M_Float (read (x:xs)))
                                   else getMsg v
                           else getMsg v
       where
             (num,rest) = span isDigit xs
             (num',rest') = span isDigit (tail rest)
             (num'',rest'') = span isDigit (x:xs)
             (num''',rest''') = span isDigit (tail rest'')

 getReadFloatVector (x:xs) v i
    = do
        if x == '-'
           then if rest /= ""
                   then if head rest == '.'
                           then if rest' /= ""
                                   then getMsg2 v i
                                   else return (M_Float (-1.0 * (read xs)))
                           else getMsg2 v i
                   else getMsg2 v i
           else if x == '+'
                   then if rest /= ""
                           then if head rest == '.'
                                   then if rest' /= ""
                                           then getMsg2 v i
                                           else return (M_Float (read xs))
                                   else getMsg2 v i
                           else getMsg2 v i
                   else if rest'' /= ""
                           then if head rest'' == '.'
                                   then if rest''' /= ""
                                           then getMsg2 v i
                                           else return (M_Float (read (x:xs)))
                                   else getMsg2 v i
                           else getMsg2 v i
       where
            (num,rest) = span isDigit xs
            (num',rest') = span isDigit (tail rest)
            (num'',rest'') = span isDigit (x:xs)
            (num''',rest''') = span isDigit (tail rest'')

 fillGarbage [] acc = return acc
 fillGarbage ((Vector v d):ts) acc 
          = do (M_Int dim) <- evalExp d []
	       vs <- fillVector dim []
	       acc' <- fillGarbage ts (acc ++ [(v,M_Vector vs)])
	       return acc'
 fillGarbage ((Simple v):ts) acc 
          = do
	      acc' <- fillGarbage ts (acc ++ [(v,M_Garbage)])
	      return acc'
 
 fillVector 0 ms = return ms
 fillVector d ms = do
                      ms' <- fillVector (d-1) (M_Garbage:ms)
		      return ms'
 
 evalExp (Factor f) ms        = do 
                                  f' <- evalFactor f ms
				  return f'
 evalExp (E_Mais e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalPlus e1' e2'
 evalExp (E_Menos e1 e2) ms   = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalMinus e1' e2'  
 evalExp (E_Prod e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalTimes e1' e2'  
 evalExp (E_Pot e1 e2) ms     = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalFactor e2 ms
				  evalPot e1' e2'  
 evalExp (E_IPot e1 e2) ms     = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalFactor e2 ms
				  evalIPot e1' e2'  
 evalExp (E_Div e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalDiv e1' e2'  
 evalExp (E_IDiv e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalIDiv e1' e2'  
 evalExp (E_Resto e1 e2) ms    = do
                                   e1' <- evalExp e1 ms
		        	   e2' <- evalFactor e2 ms
		 		   evalResto e1' e2'  
 evalExp (E_Igual e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalEq e1' e2'  
 evalExp (E_Maior e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalGt e1' e2'  
 evalExp (E_Menor e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalLt e1' e2'  
 evalExp (E_Maiorig e1 e2) ms    = do
                                     e1' <- evalExp e1 ms
				     e2' <- evalExp e2 ms
				     evalGeq e1' e2'  
 evalExp (E_Menorig e1 e2) ms  = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalLeq e1' e2'  
 evalExp (E_Dif e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalNeq e1' e2'  
 evalExp (E_E e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalAnd e1' e2'  
 evalExp (E_Ou e1 e2) ms    = do
                                  e1' <- evalExp e1 ms
				  e2' <- evalExp e2 ms
				  evalOr e1' e2'  
 evalExp (E_Nao e) ms   = do
                            e' <- evalExp e ms
		            evalNot e'  

 evalFactor (E_Int n) ms = do
                             return (M_Int n)
 evalFactor (E_Float n) ms = do
                               return (M_Float n)
 evalFactor (E_Var v) ms = do
                               v' <- seekVarValue ms v
                               return v'
 evalFactor (E_Boolean b) ms = do
                                 return (M_Boolean b)
 evalFactor (E_Vector v e) ms = do 
                                    (M_Int i) <- evalExp e ms
                                    v' <- seekVectorValue ms v (fromInteger i)
				    return v'
 evalFactor (E_Str s) ms = do
                             return (M_Str s)
 evalFactor (E_Par e) ms = do
                             e' <- evalExp e ms
			     return e'
 evalFactor (E_Pos e) ms = do
                             e' <- evalPosFactor e ms
			     return e'
 evalFactor (E_Neg e) ms = do
                             e' <- evalNegFactor e ms
			     return e'
 evalFactor (E_StrComp s) ms = do
                                 s' <- valueStrFactor s ms
                                 return (M_Int (fromInt(length s')))
 evalFactor (E_StrUlt s) ms = do
                                 s' <- valueStrFactor s ms
                                 return (M_Str ([last s']))
 evalFactor (E_StrConcat s1 s2) ms = do
                                       s1' <- valueStrFactor s1 ms
                                       s2' <- valueStrFactor s2 ms
                                       return (M_Str (s1' ++ s2'))
 evalFactor (E_StrCompara s1 s2) ms = do
                                       s1' <- valueStrFactor s1 ms
                                       s2' <- valueStrFactor s2 ms
				       s3' <- convert(compare s1' s2')
				       return s3'
 evalFactor (E_StrCopia s) ms = do
                                  s' <- valueStrFactor s ms
                                  return (M_Str s')
 evalFactor (E_StrPrim s) ms = do
                                 s' <- valueStrFactor s ms
				 if s' /= "" 
			            then do return (M_Str ([head s'])) 
				    else error ("String vazia na funcao strprim.")
 evalFactor (E_StrCalda s) ms = do
                                  s' <- valueStrFactor s ms
				  if s' /= "" 
			            then return (M_Str (tail s'))
				    else error ("String vazia na funcao strresto.")

 evalFactor (E_StrElem s n) ms = do
                                   s' <- valueStrFactor s ms
				   n' <- valueIntFactor n ms
				   if n' < (length s')
                                      then do return (M_Str [s'!!n'])
				      else error ("String com tamanho inferior ao indice na funcao strelem.")
 
 evalFactor (E_StrNPrim s n) ms = do
                                   s' <- valueStrFactor s ms
				   n' <- valueIntFactor n ms
				   if n' >= 0 
                                      then do return (M_Str (take n' s'))
				      else error ("Indice inexistente na funcao strenprim.")
 evalFactor (E_StrNResto s n) ms = do
                                      s' <- valueStrFactor s ms
			   	      n' <- valueIntFactor n ms
				      if n' >= 0 
                                          then do return (M_Str (drop n' s'))
				          else error ("Indice inexistente na funcao strnresto.")
 evalFactor (E_Formatar n1 n2) ms = do
			   	       n1' <- evalExp n1 ms
				       n1'' <- valueNumFactor n1'
				       n2' <- valueIntFactor n2 ms
				       nr <- form (show n1'') n2' 
				       if n2' > 0 
                                          then do return nr
				          else error ("Indice incorreto na funcao formatar.")
				       
				       
 evalFactor (E_RaizQuad e) ms = do
                                  e' <- evalExp e ms
				  e'' <- valueNumFactor e'
				  if e'' >= 0
				     then return (M_Float (sqrt e''))
				     else error ("Numero negativo na funcao raiz.")
 evalFactor (E_Seno e) ms = do
                              e' <- evalExp e ms
			      e'' <- valueNumFactor e'
			      return (M_Float (sin e''))
 evalFactor (E_Coseno e) ms = do
                                e' <- evalExp e ms
		                e'' <- valueNumFactor e'
			        return (M_Float (cos e''))
 evalFactor (E_Tangente e) ms = do
                                  e' <- evalExp e ms
			          e'' <- valueNumFactor e'
			          return (M_Float (tan e''))
 evalFactor (E_Exp e) ms = do
                             e' <- evalExp e ms
			     e'' <- valueNumFactor e'
			     return (M_Float (exp e''))
 evalFactor (E_Abs e) ms = do
                             e' <- evalExp e ms
			     e'' <- evalAbs e'
			     e''' <- valueNumFactor e''
			     return e''
 evalFactor (E_Log e) ms = do
                             e' <- evalExp e ms
		             e'' <- valueNumFactor e'
			     if e'' >= 0
		                 then return (M_Float (log e''))
				 else error ("Numero negativo na funcao log.")
 evalFactor (E_Pi) ms = do
		          return (M_Float (pi))
 evalFactor (E_IntReal e) ms = do
                                 e' <- evalExp e ms
		                 e'' <- valueNumFactor e'
		                 return (M_Float e'')
 evalFactor (E_RealInt e) ms = do
                                 e' <- evalExp e ms
		                 e'' <- valueFloatFactor e'
		                 return (M_Int e'')

 form (n1:n1s) n2 = do
		      if (head rest) == '.'
		          then 
			      return (M_Str (n1' ++ "." ++ form2 n1'' n2))
			  else error ("Numero invalido na funcao formatar.")	
                      where (n1',rest) = span isDigit (n1:n1s)
		            (n1'',rest') = span isDigit (tail rest)
 
 form2 [] n2 = if n2==0
                  then ""
		  else '0' : (form2 [] (n2-1))
 form2 (n1:n1s) 0 = ""
 form2 (n1:n1s) n2 = n1 : (form2 n1s (n2-1))
 
 convert r = do
               if r == GT
	          then return (M_Str "maior")
		  else if r == LT
		          then return (M_Str "menor")
		          else return (M_Str "igual")
 
 valueFloatFactor (M_Float n) = do
                                  return (round n)
 valueFloatFactor _ = do
                        error ("Erro Interno no. 105.")

 valueNumFactor (M_Float n) = do
                                return n
 valueNumFactor (M_Int n) = do
                              return (fromInteger n)
 valueNumFactor _ = do
                      error ("Erro Interno no. 106.")

 valueIntFactor (E_Var v) ms = do
                                 v' <- seekVarValue ms v
                                 v'' <- getIntValue v'
                                 return v''
 valueIntFactor (E_Vector v e) ms = do
                                      (M_Int i) <- evalExp e ms
                                      v' <- seekVectorValue ms v (fromInteger i)
                                      v'' <- getIntValue v'
                                      return v''
 valueIntFactor (E_Int e) ms = do
                                 return (fromInteger e)
 valueIntFactor _ ms = do
                         error ("Argumento com valor invalido.")

 getIntValue (M_Int e) = do
                            return (fromInteger e)
 getIntValue _ = do
                   error ("Erro Interno no. 110.")

 valueStrFactor (E_Var v) ms = do
                                 v' <- seekVarValue ms v
                                 v'' <- getStrValue v'
                                 return v''
 valueStrFactor (E_Vector v e) ms = do
                                      (M_Int i) <- evalExp e ms
                                      v' <- seekVectorValue ms v (fromInteger i)
                                      v'' <- getStrValue v'
                                      return v''
 valueStrFactor (E_Str e) ms = do
                                 return e
 valueStrFactor _ ms = do
                         error ("Erro Interno no. 107.")

 getStrValue (M_Str e) = do
                      return e
 getStrValue _ = do
              error ("Erro Interno no. 108.")

 evalPosFactor (E_Int e) ms = do
		                return (M_Int e)
 evalPosFactor (E_Float e) ms = do
		                 return (M_Float e) 
 evalPosFactor (E_Var v) ms = do
                                 v' <- seekVarValue ms v
				 v'' <- evalPosVarFactor v' v ms
		                 return v''
 evalPosFactor (E_Vector v e) ms = do
                                     (M_Int i) <- evalExp e ms
				     v' <- seekVectorValue ms v (fromInteger i)
				     v'' <- evalPosVarFactor v' v ms
				     return v''
 evalPosFactor _ ms = do
                        error ("Operador positivo atribuido incorretamente. ")
 
 evalPosVarFactor (M_Int n) v ms = do
                                    return (M_Int n)
 evalPosVarFactor (M_Float n) v ms = do
                                      return (M_Float n)
 evalPosVarFactor _ v ms = do
                              error("Operador positivo atribuido incorretamente a variavel " ++ v ++ ".")

 evalNegFactor (E_Int n) ms = do
                               return (M_Int (-1 * n))
 evalNegFactor (E_Float n) ms = do
                                 return (M_Float (-1.0 * n))
 evalNegFactor (E_Var v) ms = do
                               v' <- seekVarValue ms v
			       v'' <- evalNegVarFactor v' v ms
                               return v''
 evalNegFactor (E_Vector v e) ms = do
                                    (M_Int i) <- evalExp e ms
				    v' <- seekVectorValue ms v (fromInteger i)
				    v'' <- evalNegVarFactor v' v ms
				    return v''
 evalNegFactor _ ms = do
                        error ("Operador negativo atribuido incorretamente. ")
 evalNegVarFactor (M_Int n) v ms = do
                                    return (M_Int (-1 * n))
 evalNegVarFactor (M_Float n) v ms = do
                                      return (M_Float (-1.0 * n))
 evalNegVarFactor _ v ms = do
                              error("Operador negativo atribuido incorretamente a variavel " ++ v ++ ".")
                                
 evalIncrVar v ms = do
                    v'<- seekVarValue ms v
		    v''<- evalPlus v' (M_Int 1)
		    return v''

 evalIncrVector v ms i  = do
                    v'<- seekVectorValue ms v i
		    v''<- evalPlus v' (M_Int 1)
		    return v''

 evalDecrVar v ms = do
                    v'<- seekVarValue ms v
		    v''<- evalMinus v' (M_Int 1)
		    return v''

 evalDecrVector v ms i  = do
                    v'<- seekVectorValue ms v i
		    v''<- evalMinus v' (M_Int 1)
		    return v''
 
 seekVarValue [] v = error "Erro Interno No. 101.\nPor favor, entre em contato com os desenvolvedores do sistema."
 seekVarValue ((m,n):ms) v | m==v      = do
                                           r <- checkSimpleGarbage (checkMemType n) v n
					   return r
                           | otherwise = do
			                   r <- seekVarValue ms v
					   return r

 seekVectorValue [] v i = error "Erro Interno No. 102.\nPor favor, entre em contato com os desenvolvedores do sistema."
 seekVectorValue ((m,M_Vector vs):ms) v i 
     | m==v && i >= 0 && i < (length vs) = do
                                             r <- checkVectorGarbage v (checkMemType (vs !! i)) vs i
					     return r
     | m==v && (i < 0 || i >= (length vs)) = error ("Vetor " ++ m ++ "[" ++ show i ++ "] indexado fora dos limites.")
     | otherwise = do
                    r <- seekVectorValue ms v i
		    return r
 seekVectorValue (m:ms) v i = do
                                r <- seekVectorValue ms v i
				return r

 checkSimpleGarbage T_GARBAGE v  val = error ("Variável " ++ v ++ " não inicializada.")
 checkSimpleGarbage value v val = return val

 checkVectorGarbage v T_GARBAGE vs i = error ("Variável " ++ v ++ "[" ++ show i ++ "] não inicializada.")
 checkVectorGarbage v value vs i = return (vs !! i)
 
 evalUpdate l v n' = evalUpdate' l v n' []
   where evalUpdate'[] v n' acc = do
                                     return (acc ++ [(v,n')])
         evalUpdate' ((m,n):ms) v n' acc 
	      | (m == v) = do
	                      return (acc ++ [ (m,n') ] ++ ms)
              | otherwise = do 
	                      acc' <- evalUpdate' ms v n' (acc ++ [(m,n)])
			      return acc'

 evalVectorUpdate l v val i = evalVectorUpdate' l v val i []
    where evalVectorUpdate' [] v val i acc  = error "Erro Interno N0. 103.\nPor favor, entre em contato com os desenvolvedores do sistema."
          evalVectorUpdate' ((v', M_Vector vals) : ms) v val i acc  
	     = if v' == v 
	         then 
	            if i < 0 || i >= len
	              then error ("Vetor " ++ v ++ "[" ++ show i ++ "] indexado fora dos limites.")
		      else do
			      return (acc ++ [(v', M_Vector vals')] ++ ms)
	         else
		      do 
		         acc' <- evalVectorUpdate' ms v val i (acc++[(v', M_Vector vals)])
			 return acc'
	        where len = length vals
		      prefix = take i vals
		      sufix = drop (i+1) vals
		      vals' = prefix ++ [val] ++ sufix
          evalVectorUpdate' (m : ms) v val i acc 
	         = do 
		         acc' <- evalVectorUpdate' ms v val i (acc++[m])
			 return acc'
 
 evalPlus (M_Int m) (M_Int n) = return (M_Int (m+n))
 evalPlus (M_Float m) (M_Float n) = return (M_Float (m+n))
 evalPlus (M_Int m) (M_Float n) = return (M_Float ((fromInteger m)+n))
 evalPlus (M_Float m) (M_Int n) = return (M_Float (m+(fromInteger n)))
 evalMinus (M_Int m) (M_Int n) = return (M_Int (m-n))
 evalMinus (M_Float m) (M_Float n) = return (M_Float (m-n))
 evalMinus (M_Int m) (M_Float n) = return (M_Float ((fromInteger m)-n))
 evalMinus (M_Float m) (M_Int n) = return (M_Float (m-(fromInteger n)))
 evalTimes (M_Int m) (M_Int n) = return (M_Int (m*n))
 evalTimes (M_Float m) (M_Float n) = return (M_Float (m*n))
 evalTimes (M_Int m) (M_Float n) = return (M_Float ((fromInteger m)*n))
 evalTimes (M_Float m) (M_Int n) = return (M_Float (m*(fromInteger n)))
 evalPot (M_Int m) (M_Int n) = return (M_Float ((fromInteger m)**(fromInteger n)))
 evalPot (M_Int m) (M_Float n) = return (M_Float ((fromInteger m)**n))
 evalPot (M_Float m) (M_Int n) = return (M_Float (m**(fromInteger n)))
 evalPot (M_Float m) (M_Float n) = return (M_Float (m**n))
 evalIPot (M_Int m) (M_Int n) = return (M_Int (m^n))
 evalIPot (M_Int m) (M_Float n) = return (M_Int (round((fromInteger m)**n)))
 evalIPot (M_Float m) (M_Int n) = return (M_Int (round(m**(fromInteger n))))
 evalIPot (M_Float m) (M_Float n) = return (M_Int (round(m**n)))
 evalDiv (M_Int m) (M_Int n) = return (M_Float ((fromInteger m)/(fromInteger n)))
 evalDiv (M_Float m) (M_Float n) = return (M_Float (m/n))
 evalDiv (M_Int m) (M_Float n) = return (M_Float ((fromInteger m)/n))
 evalDiv (M_Float m) (M_Int n) = return (M_Float (m/(fromInteger n)))
 evalIDiv (M_Int m) (M_Int n) = return (M_Int (div m n))
 evalIDiv (M_Float m) (M_Float n) = return (M_Int (round(m/n)))
 evalIDiv (M_Int m) (M_Float n) = return (M_Int (round ((fromInteger m)/n)))
 evalIDiv (M_Float m) (M_Int n) = return (M_Int (round (m/(fromInteger n))))
 evalResto (M_Int m) (M_Int n) = return (M_Int (mod m n))
 evalEq (M_Int m) (M_Int n) = return (M_Boolean (m==n))
 evalEq (M_Boolean m) (M_Boolean n) = return (M_Boolean (m==n))
 evalEq (M_Float m) (M_Float n) = return (M_Boolean (m==n))
 evalEq (M_Str m) (M_Str n) = return (M_Boolean (m==n))
 evalGt (M_Int m) (M_Int n) = return (M_Boolean (m>n))
 evalGt (M_Float m) (M_Float n) = return (M_Boolean (m>n))
 evalGt (M_Str m) (M_Str n) = return (M_Boolean (m>n))
 evalLt (M_Int m) (M_Int n) = return (M_Boolean (m<n))
 evalLt (M_Float m) (M_Float n) = return (M_Boolean (m<n))
 evalLt (M_Str m) (M_Str n) = return (M_Boolean (m<n))
 evalGeq (M_Int m) (M_Int n) = return (M_Boolean (m>=n))
 evalGeq (M_Float m) (M_Float n) = return (M_Boolean (m>=n))
 evalGeq (M_Str m) (M_Str n) = return (M_Boolean (m>=n))
 evalLeq (M_Int m) (M_Int n) = return (M_Boolean (m<=n))
 evalLeq (M_Float m) (M_Float n) = return (M_Boolean (m<=n))
 evalLeq (M_Str m) (M_Str n) = return (M_Boolean (m<=n))
 evalNeq (M_Int m) (M_Int n) = return (M_Boolean (m/=n))
 evalNeq (M_Float m) (M_Float n) = return (M_Boolean (m/=n))
 evalNeq (M_Str m) (M_Str n) = return (M_Boolean (m/=n))
 evalAnd (M_Boolean m) (M_Boolean n) = return (M_Boolean (m&&n))
 evalOr (M_Boolean m) (M_Boolean n) = return (M_Boolean (m||n))
 evalNot (M_Boolean m) = return (M_Boolean (not(m)))
 evalAbs (M_Float m) = return (M_Float (abs m))
 evalAbs (M_Int m) = return (M_Int (abs m))

 fromMem (M_Int n) = show n
 fromMem (M_Float n) = show n
 fromMem (M_Var v) = v
 fromMem (M_Boolean b) = if b then "verdadeiro" else "falso"
 fromMem (M_Str s) = s
 fromMem (M_Vector l) = "[" ++ concat (map fromMem l) ++ "]"
