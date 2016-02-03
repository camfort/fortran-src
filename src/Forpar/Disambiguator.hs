module Forpar.Disambiguator where

import Forpar.AST

groupBlocks :: [ ProgramUnit () ] -> [ ProgramUnit () ]
groupBlocks pus = map aux pus 
  where
    aux pu =
      case pu of
        PUMain a s n bs -> PUMain a s n $ groupBlocks' bs
        PUSubroutine a s n as bs -> PUSubroutine a s n as $ groupBlocks' bs
        PUFunction a s r n as bs -> PUFunction a s r n as $ groupBlocks' bs
        bd@(PUBlockData _ _ _ _) -> bd
        _ -> error "Grouping blocks for unknown Program Unit"

groupBlocks' :: [ Block () ] -> [ Block () ]
groupBlocks' [] = []
groupBlocks' bs@((BlStatement _ _ _ (StIfThen _ _ _)):_) = 
  let (bif, bs') = extractIfBlock bs in bif : groupBlocks' bs'
groupBlocks' (b:bs) = b : groupBlocks' bs

extractIfBlock :: [ Block () ] -> (Block (), [ Block () ])
extractIfBlock ((bif@(BlStatement _ _ _ (StIfThen _ _ _)):bs)) = 
  let gbs = groupBlocks' bs 
      (conds, blocks, rest) = collectIfComponents (bif:gbs)
      lastBlock = last . last $ blocks
      span = getTransSpan bif lastBlock in
    (BlIf () span (getLabel bif) conds blocks, rest)
extractIfBlock _ = error "Head needs to be an IF-THEN statement"

collectIfComponents :: [ Block () ] -> ([ Maybe (Expression ()) ], [ [ Block () ] ], [ Block () ])
collectIfComponents bs =
  case bs of 
    (BlStatement _ _ _ (StIfThen _ _ e)):rest -> aux (Just e) rest
    (BlStatement _ _ _ (StElsif _ _ e)):rest -> aux (Just e) rest
    (BlStatement _ _ _ (StElse _ _)):rest -> aux Nothing rest
    (BlStatement _ _ _ (StEndif _ _)):rest -> ([], [], rest)
    _ -> error "Mismatch IF statement"
  where 
    aux e bs = 
      let (bs', rest') = collectStandardBlocks bs
          (cs, bs'', rest'') = collectIfComponents rest'
      in (e:cs, bs':bs'', rest'')

collectStandardBlocks :: [ Block () ] -> ([ Block () ], [ Block () ])
collectStandardBlocks all@((BlStatement _ _ _ (StElsif _ _ e)):_) = ([], all)
collectStandardBlocks all@((BlStatement _ _ _ (StElse _ _)):_) = ([], all)
collectStandardBlocks all@(b@(BlStatement _ _ _ (StEndif _ _)):_) = ([ b ], all)
collectStandardBlocks (x:xs) = 
  let (bs, rest) = collectStandardBlocks xs in (x : bs, rest)
