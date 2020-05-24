 module EvalProgram (execProg )
 where
 import UalTypes
 import PrelNum 
 import IO
 import Grammar 
 import ParseTypes
 import EvalExp
 import CheckProgram

 execProg (Program v ts cs) ms = do
		   ms' <- evalCmd cs ms ts (0,"s")
                   return ms' 
 
 evalCmd [] ms ts (n1,st)
   = do
        return (ms,st)

 evalCmd ((Atrib (Simple v) e) : cs) ms ts (n1,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
	          e' <- evalExp e ms
                  ms' <- evalUpdate ms v e'
                  (ms'',st') <- evalCmd cs ms' ts (n1,st)
                  return (ms'',st')

 evalCmd ((Atrib (Vector v e') e) : cs) ms ts (n1,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                   (M_Int i) <- evalExp e' ms
	           val <- evalExp e ms
                   ms'  <- evalVectorUpdate ms v val (fromInteger i)
                   (ms'',st') <- evalCmd cs ms' ts (n1,st)
                   return (ms'',st')

 evalCmd ((E_Incr (Simple v)) : cs) ms ts (n1,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  val <- evalIncrVar v ms
                  ms' <- evalUpdate ms v val
                  (ms'',st') <- evalCmd cs ms' ts (n1,st)
                  return (ms'',st')

 evalCmd ((E_Incr (Vector v e')) : cs) ms ts (n1,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  (M_Int i) <- evalExp e' ms
	          val <- evalIncrVector v ms (fromInteger i) 
                  ms'  <- evalVectorUpdate ms v val (fromInteger i)
                  (ms'',st') <- evalCmd cs ms' ts (n1,st)
                  return (ms'',st')

 evalCmd ((E_Decr (Simple v)) : cs) ms ts (n1,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  val <- evalDecrVar v ms
                  ms' <- evalUpdate ms v val
                  (ms'',st') <- evalCmd cs ms' ts (n1,st)
                  return (ms'',st')

 evalCmd ((E_Decr (Vector v e')) : cs) ms ts (n1,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  (M_Int i) <- evalExp e' ms
	          val <- evalDecrVector v ms (fromInteger i) 
                  ms'  <- evalVectorUpdate ms v val (fromInteger i)
                  (ms'',st') <- evalCmd cs ms' ts (n1,st)
                  return (ms'',st')
 
 evalCmd ((E_Se e c) : cs) ms ts (n1,st)
      = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
		  e' <- evalExp e ms
                  (M_Boolean b) <- evalEq e' (M_Boolean True)
		  if b 
		     then do
		             (ms',st') <- evalCmd c ms ts (n1,st)
			     if (last st') == 'n'
				 then do (ms'',st'') <- evalCmd [] ms' ts (n1,st')
			                 return (ms'',st'')
				 else do 
					 if (last st') == 'c'
				            then do (ms'',st'') <- evalCmd [] ms' ts (n1,"s")
					            return (ms'',st'')
					    else do (ms'',st'') <- evalCmd cs ms' ts (n1,st')
				                    return (ms'',st'')
	             else do (ms',st') <- evalCmd cs ms ts (n1,st)
		             return (ms',st')

 evalCmd ((E_Senao e c1 c2) : cs) ms ts (n1,st)
      = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  e' <- evalExp e ms
                  (M_Boolean b) <- evalEq e' (M_Boolean True)
	          if b 
	             then do 
		            (ms',st') <- evalCmd c1 ms ts (n1,st)
                            if (last st') == 'n'
			        then do (ms'',st'') <- evalCmd [] ms' ts (n1,st')
		                        return (ms'',st'')
				else do if (last st') == 'c' 
					    then do (ms'',st'') <- evalCmd [] ms' ts (n1,"s")
					            return (ms'',st'')
				            else do (ms'',st'') <- evalCmd cs ms' ts (n1,st')
				                    return (ms'',st'')
	             else do 
			    (ms',st') <- evalCmd c2 ms ts (n1,st)
                            if (last st') == 'n' 
			       then do (ms'',st'') <- evalCmd [] ms' ts (n1,st')
		                       return (ms'',st'')
			       else do  if (last st') == 'c'
				            then do (ms'',st'') <- evalCmd [] ms' ts (n1,"s")
					            return (ms'',st'')
					    else do (ms'',st'') <- evalCmd cs ms' ts (n1,st')
			       	                    return (ms'',st'')

 evalCmd ((E_Enquanto e c) : cs) ms ts (n1,st)
   = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  (ms',st') <- loopWhile e c ms ts (n1,st)
                  (ms'',st'') <- evalCmd cs ms' ts (n1,st')
                  return (ms'',st'')

 evalCmd ((E_Faca c e) : cs) ms ts (n1,st)
   = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  (ms',st') <- evalCmd c ms ts (n1+1,st)
                  (ms'',st'') <- loopDo c e ms' ts (n1,st')
                  (ms''',st''') <- evalCmd cs ms'' ts (n1,st'')
                  return (ms''',st''')
 
 evalCmd ((E_Para li lf e c) : cs) ms ts (n1,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  (ms',st')   <- evalCmd [li] ms ts (n1,st)
                  (ms'',st'')  <- loopFor lf e c ms' ts (n1,st')
                  (ms''',st''') <- evalCmd cs ms'' ts (n1,st'')
                  return (ms''',st''')

 evalCmd ((E_Saia) : cs) ms ts (n1,st)
    = do
	 (ms',st') <- evalCmd [] ms ts (n1,"t")
	 return (ms',st')
 
 evalCmd ((E_Continue) : cs) ms ts (n1,st) 
    = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
	          if n1==0
	              then do 
	                     error ("Comando continue utilizado em local improprio.")
	              else do
	                     (ms',st') <- evalCmd [] ms ts (n1,"c")
	                     return (ms',st')

 evalCmd ((E_Pare) : cs) ms ts (n1,st) 
    = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
	           if n1==0
	              then do 
	                     error ("Comando pare utilizado em local improprio.")
		      else do (ms',st') <- evalCmd [] ms ts (n1,"n")
	                      return (ms',st')
 
 evalCmd ((E_Imprima []) : cs) ms ts (n1,st) 
    = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                   (ms',st') <- evalCmd cs ms ts (n1,st)
                   return (ms',st')

 evalCmd ((E_Imprima (e:es)) : cs) ms ts (n1,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  x <- evalExp e ms
                  y <- case checkMemType x of 
                            T_INT ->  do
                                        putStr (show (getInt (fromMtoT x)))
                            T_REAL -> do
                                        putStr (show (getFloat (fromMtoT x)))
                            T_STRING -> do
                                        putStr (getStr (fromMtoT x))
	                    T_GARBAGE -> error ("Variável " ++ fromExpExp e ++ " não inicializada.")
		            _         -> error ("Tipo não permitido em impressão: " ++ fromExpExp e ++ ".")
                  (ms',st') <- evalCmd ((E_Imprima es) : cs) ms ts (n1,st)
                  return (ms',st')
               where getInt (E_Int n) = n
                     getFloat (E_Float f) = f
                     getStr (E_Str s) = s

 evalCmd ((E_Leia ((Factor (E_Var v)))) : cs) ms ts (n1,st)
   = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  x <- readLine
                  (l,st') <- case getVarType v ts of
                            T_INT -> do
		                        x' <- getReadInt x v
		                        ms' <- evalUpdate ms v x'
                                        evalCmd cs ms' ts (n1,st)
                            T_REAL -> do
		                         x' <- getReadFloat x v
		                         ms' <- evalUpdate ms v x'
                                         evalCmd cs ms' ts (n1,st)
                            T_STRING -> do
		                           ms' <- evalUpdate ms v (M_Str x)
                                           evalCmd cs ms' ts (n1,st)
	                    _        -> error "Erro interno No. 104.\nPor favor, entre em contato com os desenvolvedores do sistema."
                  return (l,st')
                where readLine = isEOF >>= \ eof ->
                                  if eof then return []
                                  else getChar >>= \ c ->
                                        if c `elem` ['\n','\r'] then
                                            return []
                                        else
                                            readLine   >>= \ cs ->
                                            return (c:cs)

 evalCmd  (E_Leia  (Factor (E_Vector v e)) : cs) ms ts (n1,st) 
   = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  x <- readLine
	          (M_Int i) <- evalExp e ms
                  (l,st') <- case getVarType v ts of
                            T_INTVECTOR    -> do
                                                 x'<- getReadIntVector x v e
    				                 ms' <- evalVectorUpdate ms v x' (fromInteger i)
                                                 evalCmd cs ms' ts (n1,st)
                            T_REALVECTOR   -> do
                                                 x' <- getReadFloatVector x v e
				                 ms' <- evalVectorUpdate ms v x' (fromInteger i)
			                         evalCmd cs ms' ts (n1,st)
                            T_STRINGVECTOR -> do
		                                 ms' <- evalVectorUpdate ms v (M_Str x) (fromInteger i)
                                                 evalCmd cs ms' ts (n1,st)
	                    _              -> error "Erro interno No. 109.\nPor favor, entre em contato com os desenvolvedores do sistema."
                  return (l,st')
                where readLine = isEOF >>= \ eof ->
                                  if eof then return []
                                   else getChar >>= \ c ->
                                        if c `elem` ['\n','\r'] then
                                            return []
                                        else
                                            readLine   >>= \ cs ->
                                            return (c:cs)

 loopWhile e c ms ts (n1,st)
    = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  if st == "n" 
                     then do return (ms,"s")
	             else do
	                     e' <- evalExp e ms
                             (M_Boolean b) <- evalEq e' (M_Boolean True)
	                     if b
                                 then do (ms',st') <- evalCmd c ms ts (n1+1,"s")
                                         (ms'',st'') <- loopWhile e c ms' ts (n1,st')
                                         return (ms'',st'')
	                         else do return (ms, "s")

 loopFor lf e c ms ts (n1,st)
    = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  if st == "n"
	             then do return (ms,"s")
	             else do
		             lf' <- evalExp lf ms
                             (M_Boolean b) <- evalEq lf' (M_Boolean True)
	                     if b
                                then do 
			                (ms',st')   <- evalCmd c ms ts (n1+1,"s")
                                        (ms'',st'')  <- evalCmd [e] ms' ts (n1,st')
		                        (ms''',st''') <- loopFor lf e c ms'' ts (n1,st'')
                                        return (ms''',st''')
		                else do return (ms,"s")	

 loopDo c e ms ts (n1,st)
    = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalCmd [] ms ts (n1,st)
                  return (ms',st')
           else do		  
                  if st == "n"
	             then do 
	                     return (ms,"s")
	             else do
	  	             e' <- evalExp e ms
                             (M_Boolean b) <- evalEq e' (M_Boolean True)
	                     if b
                                then do 
			                (ms',st') <- evalCmd c ms ts (n1+1,"s")
                                        (ms'',st'') <- loopDo c e ms' ts (n1,st')
                                        return (ms'',st'')
	                        else do 
			                return (ms,"s")
