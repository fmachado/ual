 module GenGraph (execGraph, annProg)
 where
 import DaVinci
 import DaVinciTypes
 import UalTypes
 import PrelNum 
 import ParseTypes
 import EvalExp
 import CheckProgram
 import IO

 annProg (Program v ts cs) 
     = do 
            (n,ann) <- annCmd cs 0 []
            return (n, ann)

 annCmd [] n ann 
     = do
          return (n,ann)

 annCmd ((Atrib v e) : cs) n ann 
     = do
            (n',atrb) <- annAtrib v e n
            (n'', ann') <- annCmd cs (n'+1) (ann ++ [atrb])
	    return (n'', ann')

 annCmd ((E_Incr v) : cs) n ann 
     = do
            (n',incr) <- annIncr v n
            (n'', ann') <- annCmd cs (n'+1) (ann ++ [incr])
	    return (n'', ann')
 
 annCmd ((E_Decr v) : cs) n ann 
     = do
            (n',decr) <- annDecr v n
            (n'', ann') <- annCmd cs (n'+1) (ann ++ [decr])
	    return (n'', ann')

 annCmd ((E_AtribFor v e) : cs) n ann 
     = do
            (n',atrb) <- annAtribFor v e n
            (n'', ann') <- annCmd cs (n'+1) (ann ++ [atrb])
	    return (n'', ann')

 annCmd ((E_IncrFor v) : cs) n ann 
     = do
            (n',incr) <- annIncrFor v n
            (n'', ann') <- annCmd cs (n'+1) (ann ++ [incr])
	    return (n'', ann')
 
 annCmd ((E_DecrFor v) : cs) n ann 
     = do
            (n',decr) <- annDecrFor v n
            (n'', ann') <- annCmd cs (n'+1) (ann ++ [decr])
	    return (n'', ann')
 
 annCmd ((E_Se e c) : cs) n ann 
     = do
            (n',seEntao) <- annSe e c n
	    (n'', ann') <- annCmd cs (n'+1) (ann ++ [seEntao])
            return (n'', ann')

 annCmd ((E_Senao e c1 c2) : cs) n ann 
     = do
            (n',seSenao) <- annSeSenao e c1 c2 n
	    (n'', ann') <- annCmd cs (n'+1) (ann ++ [seSenao])
            return (n'', ann')

 annCmd ((E_Enquanto e c) : cs) n ann 
     = do
            (n', enq) <- annEnq e c n
	    (n'', ann') <- annCmd cs (n'+1) (ann ++ [enq])
            return (n'', ann')

 annCmd ((E_Para li lf e c) : cs) n ann 
     = do
            li' <- renFor li
	    e'<- renFor e
            (n',li'') <- annCmd [li'] n []
            (n'', para) <- annPara li'' lf e' c (n'+1)
	    (n''', ann') <- annCmd cs (n''+1) (ann ++ li'' ++ [para])
            return (n''', ann')
 
 annCmd ((E_Faca c e) : cs) n ann 
     = do
            (n', faca) <- annFaca c e n
	    (n'', ann') <- annCmd cs (n'+1) (ann ++ [faca])
            return (n'', ann')
 
 annCmd ((E_Imprima e) : cs) n ann 
     = do
            (n',imprima) <- annImprima e n
	    (n'', ann') <- annCmd cs (n'+1) (ann ++ [imprima])
            return (n'', ann')

 annCmd ((E_Saia) : cs) n ann 
     = do
            (n',saia) <- annSaia n
	    (n'', ann') <- annCmd cs (n'+1) (ann ++ [saia])
            return (n'', ann')
 
 annCmd ((E_Continue) : cs) n ann 
     = do
            (n',continue) <- annContinue n
	    (n'', ann') <- annCmd cs (n'+1) (ann ++ [continue])
            return (n'', ann')

 annCmd ((E_Pare) : cs) n ann 
     = do
            (n',pare) <- annPare n
	    (n'', ann') <- annCmd cs (n'+1) (ann ++ [pare])
            return (n'', ann')
 
 annCmd ((E_Leia r) : cs) n ann 
     = do
            (n',leia) <- annLeia r n
	    (n'', ann') <- annCmd cs (n'+1) (ann ++ [leia])
            return (n'', ann')

 renFor (Atrib v e) 
   = do
        return (E_AtribFor v e)

 renFor (E_Incr v) 
   = do
        return (E_IncrFor v)

 renFor (E_Decr v) 
   = do
        return (E_DecrFor v)
 
 annAtrib v e n = do
                     return (n, (makeId "eat" n,makeId "at" n, Ann_Atrib v e))

 annIncr v n = do
                     return (n, (makeId "eic" n,makeId "ic" n, Ann_Incr v))
 
 annDecr v n = do
                     return (n, (makeId "edc" n,makeId "dc" n, Ann_Decr v))

 annAtribFor v e n = do
                     return (n, (makeId "eat" n,makeId "at" n, Ann_AtribFor v e))

 annIncrFor v n = do
                     return (n, (makeId "eic" n,makeId "ic" n, Ann_IncrFor v))
 
 annDecrFor v n = do
                     return (n, (makeId "edc" n,makeId "dc" n, Ann_DecrFor v))

 annSe e c n = do
                  (n', c') <- annCmd c (n+1) []
                  return (n', (makeId "eif" n, makeId "if" n, Ann_Se e c'))

 annSeSenao e c1 c2 n = do
                           (n',c1') <- annCmd c1 (n+1) []
			   (n'',c2') <- annCmd c2 (n'+1) []
                           return (n'',(edgeid,nodeid, Ann_Senao e c1' c2'))
                        where nodeid = makeId "el" n
                              edgeid = makeId "eel" n

 annEnq e c n = do
                   (n',c') <- annCmd c (n+1) []
                   return (n', (edgeid, nodeid, Ann_Enquanto e c'))
                where nodeid = makeId "wh" n
                      edgeid = makeId "ewh" n

 annPara li lf e c n = do
                   (n',c') <- annCmd (c ++ [e]) (n+1) []
                   return (n', (edgeid, nodeid, Ann_Para li lf e c'))
                where nodeid = makeId "fr" n
                      edgeid = makeId "efr" n
 
 annFaca c e n = do
                   (n',c') <- annCmd c (n+1) []
                   return (n', (edgeid, nodeid, Ann_Faca c' e))
                where nodeid = makeId "fa" n
                      edgeid = makeId "efa" n
 
 annImprima e n = do
                     return (n, (edgeid,nodeid, Ann_Imprima e))
                  where nodeid = makeId "pt" n
                        edgeid = makeId "ept" n

 annSaia n = do
                return (n, (edgeid,nodeid, Ann_Saia))
             where nodeid = makeId "sa" n
                   edgeid = makeId "esa" n
 
 annContinue n = do
                    return (n, (edgeid,nodeid, Ann_Continue))
                 where nodeid = makeId "co" n
                       edgeid = makeId "eco" n

 annPare n = do
                    return (n, (edgeid,nodeid, Ann_Pare))
                 where nodeid = makeId "pa" n
                       edgeid = makeId "epa" n

 annLeia r n = do
                    return (n, (edgeid,nodeid, Ann_Leia r))
                 where nodeid = makeId "rd" n
                       edgeid = makeId "erd" n

 makeId labname n = labname ++ (show n)

 evalAnnGraph t [] ms ts dvpid (n1,lt,st)
   = do
        return (ms,st)

 evalAnnGraph t ((id1,id2, Ann_Atrib (Simple v) e) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          exp <- evalExp e ms
	          ms' <- evalUpdate ms v exp
	          v' <- seekVarValue ms' v
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "<-" ++ (expReduce (fromMem v') 7)),A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#96fef8"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1,id2, Ann_AtribFor (Simple v) e) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          exp <- evalExp e ms
	          ms' <- evalUpdate ms v exp
	          v' <- seekVarValue ms' v
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "<-" ++ expReduce (fromMem v') 7),A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#7af0ca"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1,id2, Ann_Incr (Simple v)) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          val <- evalIncrVar v ms
	          ms' <- evalUpdate ms v val
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "<-" ++ (fromMem val)), A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#ffe0da"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1,id2, Ann_IncrFor (Simple v)) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          val <- evalIncrVar v ms
	          ms' <- evalUpdate ms v val
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "<-" ++ (fromMem val)), A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#7af0ca"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')
 
 evalAnnGraph t ((id1,id2, Ann_Decr (Simple v)) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          val <- evalDecrVar v ms
	          ms' <- evalUpdate ms v val
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "<-" ++ (fromMem val)), A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#add0ff"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1,id2, Ann_DecrFor (Simple v)) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          val <- evalDecrVar v ms
	          ms' <- evalUpdate ms v val
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "<-" ++ (fromMem val)), A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#7af0ca"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')
 
 evalAnnGraph t ((id1,id2, Ann_Atrib (Vector v d) e) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          exp <- evalExp e ms
	          (M_Int i) <- evalExp d ms
	          ms' <- evalVectorUpdate ms v exp (fromInteger i)
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "[" ++ show i ++ "] <-" ++ (expReduce (fromMem exp) 7)),A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#96fef8"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1,id2, Ann_AtribFor (Vector v d) e) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          exp <- evalExp e ms
	          (M_Int i) <- evalExp d ms
	          ms' <- evalVectorUpdate ms v exp (fromInteger i)
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "[" ++ show i ++ "] <-" ++ (expReduce (fromMem exp) 7)),A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#7af0ca"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1,id2, Ann_Incr (Vector v d)) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          (M_Int i) <- evalExp d ms
	          exp <- evalIncrVector v ms (fromInteger i)
	          ms' <- evalVectorUpdate ms v exp (fromInteger i)
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "[" ++ show i ++ "] <-" ++ (expReduce (fromMem exp) 7)),A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#ffe0da"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1,id2, Ann_IncrFor (Vector v d)) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          (M_Int i) <- evalExp d ms
	          exp <- evalIncrVector v ms (fromInteger i)
	          ms' <- evalVectorUpdate ms v exp (fromInteger i)
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "[" ++ show i ++ "] <-" ++ (expReduce (fromMem exp) 7)),A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#7af0ca"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1,id2, Ann_Decr (Vector v d)) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          (M_Int i) <- evalExp d ms
	          exp <- evalDecrVector v ms (fromInteger i)
	          ms' <- evalVectorUpdate ms v exp (fromInteger i)
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "[" ++ show i ++ "] <-" ++ (expReduce (fromMem exp) 7)),A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#add0ff"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1,id2, Ann_DecrFor (Vector v d)) : cs) ms ts dvpid (n1,lt,st)
   = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          (M_Int i) <- evalExp d ms
	          exp <- evalDecrVector v ms (fromInteger i)
	          ms' <- evalVectorUpdate ms v exp (fromInteger i)
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "OBJECT" (v ++ "[" ++ show i ++ "] <-" ++ (expReduce (fromMem exp) 7)),A "COLOR" "yellow"]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#7af0ca"]]))
	          (ms'',st') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st)
                  return (ms'',st')

 evalAnnGraph t ((id1, id2, Ann_Se e c) : cs) ms ts dvpid (n1,lt,st)
  = do 
     if st == "t"
      then do 
            (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
            return (ms',st')
      else do
	    e' <- evalExp e ms
            (M_Boolean b) <- evalEq e' (M_Boolean True)
	    if b 
	     then do a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                     a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                     a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
                     a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#fec2a0"]]))
                     a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("et" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                     a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("et" ++ id2)) [A "EDGECOLOR" "black"]]))
                     if st == "f" && extractCmd c
	              then do (ms',st') <- evalAnnGraph t (findContinue c ++ cs) ms ts dvpid (n1,lt,st)
	                      return (ms',st')
		      else do (ms',st') <- evalAnnGraph t c ms ts dvpid (n1,lt,st)
                              if last st' == 'n'
				then do (ms'',st'') <-  evalAnnGraph t [] ms' ts dvpid (n1,lt,st')
                                        return (ms'',st'')
				else do  if (last st') == 'c'
					    then do (ms'',st'') <-  evalAnnGraph t [] ms' ts dvpid (n1,lt,"s")
                                                    return (ms'',st'')
                                            else do
			                           a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "black"]]))
                                                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A "COLOR" "yellow"]]))
                                                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A "COLOR" "#fec2a0"]]))
                                                   (ms'',st'') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st')
                                                   return (ms'',st'')
             else do
                    a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                    a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                    a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
                    a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#fec2a0"]]))
                    a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                    a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "black"]]))
                    a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "yellow"]]))
                    a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "#fec2a0"]]))
                    (ms',st') <- evalAnnGraph t cs ms ts dvpid (n1,lt,st)
                    return (ms',st')

 evalAnnGraph t ((id1, id2, Ann_Senao e c1 c2) : cs) ms ts dvpid (n1,lt,st)
  = do
     if st == "t"
      then do 
            (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
            return (ms',st')
      else do		  
            e' <- evalExp e ms
            (M_Boolean b) <- evalEq e' (M_Boolean True)
            if b 
	     then do
                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                   a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
	           a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#fec2a0"]]))
                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("et" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("et" ++ id2)) [A "EDGECOLOR" "black"]]))
                   if st == "f" && extractCmd c1
	              then do (ms',st') <- evalAnnGraph t (findContinue c1 ++ cs) ms ts dvpid (n1,lt,st)
	                      return (ms',st')
		      else do
                             (ms',st') <- evalAnnGraph t c1 ms ts dvpid (n1,lt,"s")
			     if (last st') == 'n' 
				then do (ms'',st'') <- evalAnnGraph t [] ms' ts dvpid (n1,lt,st')
				        return (ms'',st'')
			        else do if (last st') == 'c' 
				           then do (ms'',st'') <- evalAnnGraph t [] ms' ts dvpid (n1,lt,"s")
				                   return (ms'',st'')
			                   else do 				     
                                                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "black"]]))
                                                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A "COLOR" "yellow"]]))
                                                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A "COLOR" "#fec2a0"]]))
                                                   (ms'',st'') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st')
                                                   return (ms'',st'')
             else do      
                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                   a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
	           a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#fec2a0"]]))
                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ee" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ee" ++ id2)) [A "EDGECOLOR" "black"]]))
                   if st == "f" && extractCmd c2
	            then do (ms',st') <- evalAnnGraph t (findContinue c2 ++ cs) ms ts dvpid (n1,lt,st)
	                    return (ms',st')
                    else do (ms',st') <- evalAnnGraph t c2 ms ts dvpid (n1,lt,st)
		            if (last st') == 'n'
				then do (ms'',st'') <- evalAnnGraph t [] ms' ts dvpid (n1,lt,st')
				        return (ms'',st'')
			        else do if (last st') == 'c'
				            then do (ms'',st'') <- evalAnnGraph t [] ms' ts dvpid (n1,lt,"s")
				                    return (ms'',st'')
				            else do	
                                                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                                   a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "black"]]))
                                                   a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "yellow"]]))
                                                   a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "#fec2a0"]]))
                                                   (ms'',st'') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st')
                                                   return (ms'',st'')

 evalAnnGraph t ((id1, id2, Ann_Enquanto e c) : cs) ms ts dvpid (n1,lt,st)
   = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                  (ms',st') <- loopAnnWhile e c ms ts dvpid id1 id2 t (n1,lt,st)
                  (ms'',st'') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st')
                  return (ms'',st'')

 evalAnnGraph t ((id1, id2, Ann_Para li lf e c) : cs) ms ts dvpid (n1,lt,st)
   = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                  (ms',st') <- loopAnnFor lf c ms ts dvpid id1 id2 t (n1,lt,st)
                  (ms'',st'') <- evalAnnGraph t cs ms' ts dvpid (n1,lt,st')
                  return (ms'',st'')
 
 evalAnnGraph t ((id1, id2, Ann_Faca c e) : cs) ms ts dvpid (n1,lt,st)
   = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#e8ffbc"]]))
                  (ms',st') <- evalAnnGraph t c ms ts dvpid (n1+1,lt,st)
                  (ms'',st'') <- loopAnnDo c e ms' ts dvpid id1 id2 t (n1,lt,st')
                  (ms''',st''') <- evalAnnGraph t cs ms'' ts dvpid (n1,lt,st'')
                  return (ms''',st''')

 evalAnnGraph t ((id1, id2, Ann_Imprima e) : cs) ms ts dvpid (n1,lt,st)
    = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <- sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
	          p <- execPrint e ms
                  a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow", A "OBJECT" (expReduce p 15)]]))
	          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#d4c18a"]]))
                  (ms',st') <- evalAnnGraph t cs ms ts dvpid (n1,lt,st)
                  return (ms',st')

 evalAnnGraph t ((id1, id2, Ann_Saia) : cs) ms ts dvpid (n1,lt,st)
    = do
          a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
          a <- sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
	  a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "red"]]))
          (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,"t")
          return (ms',st')

 evalAnnGraph t ((id1, id2, Ann_Continue) : cs) ms ts dvpid (n1,lt,st)
    = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t lt ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  if n1 == 0
                     then do
	                     error ("Comando continue utilizado em local improprio.")
	             else do	     
	                     a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                             a <- sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                             a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
	                     a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "green"]]))
                             if st == "f"
                                then do
                                        (ms',st') <- evalAnnGraph t lt ms ts dvpid (n1,lt,"c")
                                        return (ms',st')
                                else do
                                        (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,"c")
                                        return (ms',st')

 evalAnnGraph t ((id1, id2, Ann_Pare) : cs) ms ts dvpid (n1,lt,st)
    = do
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  if n1 == 0
                     then do
	                     error ("Comando pare utilizado em local improprio.")
	             else do	     
	                     a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                             a <- sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                             a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
	                     a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "blue"]]))
		             (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,"n")
		             return (ms',st')
 
 evalAnnGraph t ((id1, id2, Ann_Leia (Factor (E_Var v))) : cs) ms ts dvpid (n1,lt,st)
   = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                  a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow", A "OBJECT" v]]))
                  x <- readLine
                  (l,st') <- case getVarType v ts of
                             T_INT -> do
		                         x' <- getReadInt x v
			                 m  <- evalUpdate ms v x'
			                 v' <- seekVarValue m v
                                         a  <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow", A "OBJECT" (fromMem v')]]))
                                         a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#c3c3c2"]]))
                                         (ms',st')<- evalAnnGraph t cs m ts dvpid (n1,lt,st)
			                 return (ms',st')
                             T_REAL -> do
                                          x' <- getReadFloat x v
			                  m  <- evalUpdate ms v x'
			                  v' <- seekVarValue m v
                                          a  <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow", A "OBJECT" (fromMem v')]]))
                                          a  <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#c3c3c2"]]))
			                  (ms',st')<- evalAnnGraph t cs m ts dvpid (n1,lt,st)
			                  return (ms',st')
                             T_STRING -> do
                                           m  <- evalUpdate ms v (M_Str x)
				           v' <- seekVarValue m v
                                           a  <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow", A "OBJECT" (expReduce (fromMem v') 15)]]))
                                           a  <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#c3c3c2"]]))
				           (ms',st')<- evalAnnGraph t cs m ts dvpid (n1,lt,st)
			                   return (ms',st')
                             _        -> error "Erro interno No. 111.\nPor favor, entre em contato com os desenvolvedores do sistema."
                  return (l,st')
                 where readLine = isEOF >>= \ eof ->
                                   if eof then return []
                                   else getChar >>= \ c ->
                                         if c `elem` ['\n','\r'] then
                                             return []
                                         else
                                            readLine   >>= \ cs ->
                                             return (c:cs)
 
 evalAnnGraph t ((id1, id2, Ann_Leia (Factor (E_Vector v e))) : cs) ms ts dvpid (n1,lt,st)
   = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  (M_Int i) <- evalExp e ms
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "yellow"]]))
                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId id1) [A "EDGECOLOR" "black"]]))
                  a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow", A "OBJECT" (v ++ "[" ++ (show i) ++ "]")]]))
                  x <- readLine
                  (l,st') <- case getVarType v ts of
                            T_INTVECTOR -> do
                                             x'          <- getReadIntVector x v e
				             m           <- evalVectorUpdate ms v x' (fromInteger i)
				             (M_Int ind) <- evalExp e m
				             v'          <- seekVectorValue m v (fromInteger ind)
                                             a           <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow", A "OBJECT" (fromMem v')]]))
	                                     a           <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#c3c3c2"]]))
                                             (ms',st')   <- evalAnnGraph t cs m ts dvpid (n1,lt,st)
				             return (ms',st')
                            T_REALVECTOR -> do
                                                x'          <- getReadFloatVector x v e
				                m           <- evalVectorUpdate ms v x' (fromInteger i)
				                (M_Int ind) <- evalExp e m
				                v'          <- seekVectorValue m v (fromInteger ind)
                                                a           <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow", A "OBJECT" (fromMem v')]]))
	                                        a           <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#c3c3c2"]]))
                                                (ms',st')   <- evalAnnGraph t cs m ts dvpid (n1,lt,st)
				                return (ms',st')
                            T_STRINGVECTOR -> do
	                                        m           <-  evalVectorUpdate ms v (M_Str x) (fromInteger i)
				                (M_Int ind) <- evalExp e m
				                v'          <- seekVectorValue m v (fromInteger ind)
                                                a           <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow", A "OBJECT" (expReduce (fromMem v') 15)]]))
	                                        a           <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#c3c3c2"]]))
                                                (ms',st')   <- evalAnnGraph t cs m ts dvpid (n1,lt,st)
				                return (ms',st')
                            _              -> error "Erro interno No. 112.\nPor favor, entre em contato com os desenvolvedores do sistema."
		  
                  return (l,st')
                where readLine = isEOF >>= \ eof ->
                                  if eof then return []
                                  else getChar >>= \ c ->
                                        if c `elem` ['\n','\r'] then
                                            return []
                                        else
                                           readLine   >>= \ cs ->
                                           return (c:cs)

 loopAnnWhile e c ms ts dvpid id1 id2 t (n1,lt,st)
    = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  if st == "n"
	             then do return (ms,"s")	  
	             else do
	                    e' <- evalExp e ms
                            (M_Boolean b) <- evalEq e' (M_Boolean True)
	                    if b 
	                       then do 
                                      a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
	                              a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#e194ad"]]))
                                      a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ed" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                      a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ed" ++ id2)) [A "EDGECOLOR" "black"]]))
                                      (ms',st') <- evalAnnGraph t c ms ts dvpid (n1+1,lt,"s")
                                      if st' == "t"  
					 then do 
                                                (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st')
                                                return (ms',st')
                                         else do		  
                                                a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                                a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "black"]]))
                                                a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "yellow"]]))
                                                a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "#e194ad"]]))
				                a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                                a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "black"]]))
                                                (ms'',st'') <- loopAnnWhile e c ms' ts dvpid id1 id2 t (n1,lt,st')
                                                return (ms'',st'')
                               else do
                                      a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
	                              a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#e194ad"]]))
                                      return (ms,"s")

 loopAnnFor e c ms ts dvpid id1 id2 t (n1,lt,st)
    = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  if st == "n"
	             then do return (ms,"s")
	             else do
	                    e' <- evalExp e ms
                            (M_Boolean b) <- evalEq e' (M_Boolean True)
	                    if b 
	                       then do 
                                      a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
	                              a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#7af0ca"]]))
                                      a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ed" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                      a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ed" ++ id2)) [A "EDGECOLOR" "black"]]))
                                      (ms',st') <- evalAnnGraph t c ms ts dvpid (n1+1,[last c],"f")
                                      if st' == "t"
	                                 then do 
                                                (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st')
                                                return (ms',st')
                                         else do		  
                                                a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                                a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "black"]]))
                                                a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "yellow"]]))
                                                a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "#7af0ca"]]))
                                                a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                                a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "black"]]))
                                                (ms'',st'') <- loopAnnFor e c ms' ts dvpid id1 id2 t (n1,lt,st')
                                                return (ms'',st'')
                               else do
                                      a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "yellow"]]))
	                              a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A  "COLOR" "#7af0ca"]]))
                                      return (ms,"s")
 
 loopAnnDo c e ms ts dvpid id1 id2 t (n1,lt,st)
    = do 
        if st == "t"
	   then do 
                  (ms',st') <- evalAnnGraph t [] ms ts dvpid (n1,lt,st)
                  return (ms',st')
           else do		  
                  if st == "n"
	             then do
	                    return (ms,"s")
	             else do
                            e' <- evalExp e ms
                            (M_Boolean b) <- evalEq e' (M_Boolean True)
                            a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                            a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("ec" ++ id2)) [A "EDGECOLOR" "black"]]))
                            a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "yellow"]]))
                            a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId ("nc" ++ id2)) [A  "COLOR" "#e8ffbc"]]))
	                    if b 
	                       then do 
                                      a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "yellow"]]))
                                      a <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId ("er" ++ id2)) [A "EDGECOLOR" "black"]]))
                                      a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "yellow"]]))
                                      a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId id2) [A "COLOR" "#e8ffbc"]]))
                                      (ms',st') <- evalAnnGraph t c ms ts dvpid (n1+1,lt,"s")
		                      (ms'',st'') <- loopAnnDo c e ms' ts dvpid id1 id2 t (n1,lt,st')
                                      return (ms'',st'')
                               else do
                                      return (ms,"s")

 extractCmd [] = False
 extractCmd ((_,_,Ann_Continue) : ns) = True
 extractCmd (n : ns) = extractCmd ns

 findContinue ((n1,n2,Ann_Continue) : ns) = [(n1,n2,Ann_Continue)]
 findContinue (n : ns) = [n] ++ findContinue ns

 sendApi t dvpid atributos  = do
                                j <- tempo t
				n <- sendCmd dvpid atributos
                                return n

 tempo 0 = return ()
 tempo n = do x <- tempo (n-1) 
              return x
 
 expReduce [] max = ""
 expReduce (e:es) 0 = "..."
 expReduce ('\n':es) 15 = expReduce es 15
 expReduce (e:es) max = if e == '\n' then [e] ++ expReduce es 15
                                     else 
					   if e == '\t' then "  " ++ expReduce es (max-2)
				                        else [e] ++ expReduce es (max-1)

 execPrint [] ms = return ""

 execPrint (e:es) ms
    = do x <- evalExp e ms
         let y = checkMemType x
         if  equalType T_INT y 
     	    then
     	       do
     	          p <- execPrint es ms
                  return (show (getInt x) ++ p) 
            else
     	       do
     	          if equalType T_REAL y 
     		     then
     		        do
     		          p <- execPrint es ms
                          return (show (getFloat x) ++ p)
     		     else if equalType T_STRING y 
     		               then
     			           do
     					p <- execPrint es ms
                                        return (getStr x ++ p)
     				else
     					error ("Tipo não permitido em impressão: " ++ fromExpExp e ++ ".")
      where getInt (M_Int n) = n
            getFloat (M_Float f) = f
            getStr (M_Str s) = s
 
 startGraph cs
    = N (NodeId "Inicio") (Type "") 
        [A "COLOR" "#ffffe3" , A "OBJECT" "Inicio" , A "_GO" "ellipse" ]
        [ genGraph cs [] [] [] [] ""]    

 genGraph [] [] ident [] pcs exp
    = E (EdgeId "edFim") (Type "") [] 
      (N (NodeId "Fimprog") (Type "") 
         [A "COLOR" "#ffffe3" , A "OBJECT" "Fimprog" , A "_GO" "ellipse"] []
      )

 genGraph [] (l:ls) ident ("0":cs) pcs exp 
    = closeWhile ident 

 genGraph [] (l:ls) ident ("1":cs) pcs exp
    = closeIf l ls ident cs pcs

 genGraph [] l (ident:idents) ("2":cs) pcs exp = refIf ident

 genGraph [] (l:ls) ident ("3":cs) pcs exp
    = closeDo l ls ident cs pcs exp

 genGraph [] (l:ls) ident ("4":cs) pcs exp 
    = closeFor ident 
 
 genGraph ((id1,id2, Ann_Atrib (Simple v) e) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#96fef8" , A "OBJECT" (v ++ "<-" ++ (expReduce (fromExpExp e) 8))]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_AtribFor (Simple v) e) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#7af0ca" , A "OBJECT" (v ++ "<-" ++ (expReduce (fromExpExp e) 8))]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_Incr (Simple v)) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#ffe0da" , A "OBJECT" (v ++ "++")]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_IncrFor (Simple v)) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#7af0ca" , A "OBJECT" (v ++ "++")]
         [genGraph cs l ident list pcs exp]
      )
 
 genGraph ((id1,id2, Ann_Decr (Simple v)) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#add0ff" , A "OBJECT" (v ++ "--")]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_DecrFor (Simple v)) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#7af0ca" , A "OBJECT" (v ++ "--")]
         [genGraph cs l ident list pcs exp]
      )
      
 genGraph ((id1,id2, Ann_Atrib (Vector v d) e) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#96fef8" , A "OBJECT" (v ++ "[" ++ (expReduce (fromExpExp d) 4)  ++ "] <-" ++ (expReduce (fromExpExp e) 8))]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_AtribFor (Vector v d) e) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#7af0ca" , A "OBJECT" (v ++ "[" ++ (expReduce (fromExpExp d) 4)  ++ "] <-" ++ (expReduce (fromExpExp e) 8))]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_Incr (Vector v d)) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#ffe0da" , A "OBJECT" (v ++ "[" ++ (expReduce (fromExpExp d) 4)  ++ "] ++")]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_IncrFor (Vector v d)) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#7af0ca" , A "OBJECT" (v ++ "[" ++ (expReduce (fromExpExp d) 4)  ++ "] ++")]
         [genGraph cs l ident list pcs exp]
      )
 
 genGraph ((id1,id2, Ann_Decr (Vector v d)) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#add0ff" , A "OBJECT" (v ++ "[" ++ (expReduce (fromExpExp d) 4)  ++ "] --")]
         [genGraph cs l ident list pcs exp]
      )
 
 genGraph ((id1,id2, Ann_DecrFor (Vector v d)) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "COLOR" "#7af0ca" , A "OBJECT" (v ++ "[" ++ (expReduce (fromExpExp d) 4)  ++ "] --")]
         [genGraph cs l ident list pcs exp]
      )
 
 genGraph ((id1,id2, Ann_Enquanto e c) : cs) l ident list pcs exp
   = E (EdgeId id1) (Type "") []
       (N (NodeId id2) (Type "")
          [A "COLOR" "#e194ad" , A "_GO" "rhombus" , A "OBJECT" (expReduce (fromExpExp e) 6)]
          [refWhile id2 , nodeWhile c (c:l) (id2:ident) ("0":list) pcs , genGraph cs l ident list cs exp]
       )
 
 genGraph ((id1,id2, Ann_Para li lf e c) : cs) l ident list pcs exp
   = E (EdgeId id1) (Type "") []
        (N (NodeId id2) (Type "")
          [A "COLOR" "#7af0ca" , A "_GO" "rhombus" , A "OBJECT" (expReduce (fromExpExp lf) 6)]
          [refFor id2 , nodeFor c (c:l) (id2:ident) ("4":list) pcs , genGraph cs l ident list cs exp]
     )

 genGraph ((id1,id2, Ann_Faca c e) : cs) l ident list pcs exp
   = E (EdgeId id1) (Type "") []
     (N (NodeId id2) (Type "")
        [A "_GO" "circle" , A "COLOR" "#e8ffbc", A "OBJECT" "faca"]
        [refDo id2 , genGraph c (cs:l) (id2:ident) ("3":list) pcs (fromExpExp e)]
     )

 genGraph ((id1,id2, Ann_Se e c) : cs) l ident list pcs exp
   = E (EdgeId id1) (Type "") []
     (N (NodeId id2) (Type "")
        [A "_GO" "rhombus" , A "OBJECT" (expReduce (fromExpExp e) 6) , A "COLOR" "#fec2a0"]
        [refIf id2 , nodeThen c (cs:l) (id2:ident) ("1":list) pcs]
     )

 genGraph ((id1,id2, Ann_Senao e c1 c2) : cs) l ident list pcs exp
   = E (EdgeId id1) (Type "") []
     (N (NodeId id2) (Type "")
       [A "_GO" "rhombus" , A "OBJECT" (expReduce (fromExpExp e) 6) , A "COLOR" "#fec2a0"]
        [nodeThen c1 l (id2:ident) ("2":list) cs , nodeElse c2 (cs:l) (id2:ident) ("1":list) pcs] 
     )

 genGraph ((id1,id2, Ann_Imprima e) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") [] 
      (N (NodeId id2) (Type "") 
         [A "ICONFILE" "/usr/local/ual/image/print.xbm" , A "COLOR" "#d4c18a" , A "OBJECT" (expReduce (printExp e) 15) , A "_GO" "icon"]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_Saia) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") [] 
      (N (NodeId id2) (Type "") 
         [A "COLOR" "red" , A "OBJECT" "Saia" , A "_GO" "circle"]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_Continue) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") [] 
      (N (NodeId id2) (Type "") 
         [A "COLOR" "green" , A "OBJECT" "Cont" , A "_GO" "circle"]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_Pare) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") [] 
      (N (NodeId id2) (Type "") 
         [A "COLOR" "blue" , A "OBJECT" "Pare" , A "_GO" "circle"]
         [genGraph cs l ident list pcs exp]
      )

 genGraph ((id1,id2, Ann_Leia v) : cs) l ident list pcs exp
    = E (EdgeId id1) (Type "") []
      (N (NodeId id2) (Type "") 
         [A "ICONFILE" "/usr/local/ual/image/read.xbm" , A "COLOR" "#c3c3c2" , A "OBJECT" (fromExpExp v) , A "_GO" "icon"]
         [genGraph cs l ident list pcs exp]
       )

 printExp [] = ""
 printExp (e:es) = fromExpExp e ++ printExp es

 refIf ident
   = E (EdgeId ("er" ++ ident)) (Type "") []
     (R (NodeId ("nc" ++ ident)))

 refWhile ident
   = E (EdgeId ("er" ++ ident)) (Type "") 
       [A "EDGEPATTERN" "dashed" , A "_DIR" "inverse"]
       (R (NodeId ("nc" ++ ident)))

 refFor ident
   = E (EdgeId ("er" ++ ident)) (Type "") 
       [A "EDGEPATTERN" "dashed" , A "_DIR" "inverse"]
       (R (NodeId ("nc" ++ ident)))
 
 refDo ident
   = E (EdgeId ("er" ++ ident)) (Type "") 
       [A "EDGEPATTERN" "dashed" , A "_DIR" "inverse"]
       (R (NodeId ("nc" ++ ident)))
 
 closeIf l ls (ident:idents) cs pcs
   = E (EdgeId ("ec" ++ ident)) (Type "") []
     (N (NodeId ("nc" ++ ident)) (Type "")
        [A "_GO" "circle" , A "COLOR" "#fec2a0"]
        [genGraph l ls idents cs pcs ""]
     )

 closeWhile (ident:_)
   = E (EdgeId ("ec" ++ ident)) (Type "") []
     (N (NodeId ("nc" ++ ident)) (Type "")
        [A "_GO" "circle" , A "COLOR" "#e194ad"] []
     )

 closeFor (ident:_)
   = E (EdgeId ("ec" ++ ident)) (Type "") []
     (N (NodeId ("nc" ++ ident)) (Type "")
        [A "_GO" "circle" , A "COLOR" "#7af0ca"] []
     )
     
 closeDo l ls (ident:idents) cs pcs e
   = E (EdgeId ("ec" ++ ident)) (Type "") []
     (N (NodeId ("nc" ++ ident)) (Type "")
        [A "COLOR" "#e8ffbc" , A "_GO" "rhombus" , A "OBJECT" (expReduce e 6)]
        [genGraph l ls idents cs pcs e]
     )

 nodeThen c l ident list pcs
   = E (EdgeId ("et" ++ head(ident))) (Type "") []
     (N (NodeId ("nt" ++ head(ident))) (Type "")
        [A "_GO" "text" , A "OBJECT" "entao"]
        [genGraph c l ident list pcs ""]
     )

 nodeElse c2 l ident list pcs
   = E (EdgeId ("ee" ++ head(ident))) (Type "") []
     (N (NodeId ("ne" ++ head(ident))) (Type "")
        [A "_GO" "text" , A "OBJECT" "senao"]
        [genGraph c2 l ident list pcs ""]
    )

 nodeWhile c l ident list pcs
  = E (EdgeId ("ed" ++ head(ident))) (Type "") []
     (N (NodeId ("no" ++ head(ident))) (Type "")
        [A "_GO" "text" , A "OBJECT" "faca"]
        [genGraph c l ident list pcs ""]
     )

 nodeFor c l ident list pcs
  = E (EdgeId ("ed" ++ head(ident))) (Type "") []
     (N (NodeId ("no" ++ head(ident))) (Type "")
        [A "_GO" "text" , A "OBJECT" "faca"]
        [genGraph c l ident list pcs ""]
     )

 execGraph ann dvpid ms ts t = do
                                  a <- sendApi t dvpid (Window (Title "UAL - UNESA Algorithmic Language"))
                                  a <- sendApi t dvpid (Window (Position 30 0))
                                  a <- sendApi t dvpid (Window (Size 460 570))
                                  a <- sendApi t dvpid (Window (ShowStatus "Projeto Final - Universidade Estacio de Sa "))
                                  a <- sendApi t dvpid (Menu (Layout(ImproveAll)))
                                  a <- sendApi t dvpid (DVSet (FontSize 10))
                                  a <- sendApi t dvpid (DVSet (GapWidth 4))
                                  a <- sendApi t dvpid (DVSet (GapHeight 4))
                                  a <- sendApi t dvpid (Menu (Layout(ImproveAll)))
				  a <- sendApi t dvpid (Graph (New[startGraph ann]))
                                  a <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId "Inicio") [A "COLOR" "yellow"]]))
       	                          a <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId "Inicio") [A "COLOR" "#ffffe3"]]))
                                  e <- evalAnnGraph t ann ms ts dvpid (0,[],"s")
                                  b <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId "edFim") [A "EDGECOLOR" "yellow"]]))
                                  b <-  sendApi t dvpid (Graph (ChangeAttr [ Edge (EdgeId "edFim") [A "EDGECOLOR" "black"]]))
                                  b <-  sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId "Fimprog") [A "COLOR" "yellow"]]))
       	                          b <- sendApi t dvpid (Graph (ChangeAttr [ Node (NodeId "Fimprog") [A "COLOR" "#ffffe3"]]))
       			          return e
