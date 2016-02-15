module Forpar.Transformations.Grouping (groupIf) where

import Forpar.AST

groupIf :: [ ProgramUnit a ] -> [ ProgramUnit a ]
groupIf pus = map aux pus 
  where
    aux pu =
      case pu of
        PUMain a s n bs -> PUMain a s n $ groupIf' bs
        PUSubroutine a s n as bs -> PUSubroutine a s n as $ groupIf' bs
        PUFunction a s r n as bs -> PUFunction a s r n as $ groupIf' bs
        bd@(PUBlockData _ _ _ _) -> bd

groupIf' :: [ Block a ] -> [ Block a ]
groupIf' [] = []
groupIf' bs@((BlStatement _ _ _ (StIfThen _ _ _)):_) = 
  let (bif, bs') = extractIfBlock bs in bif : groupIf' bs'
groupIf' (b:bs) = b : groupIf' bs

extractIfBlock :: [ Block a ] -> (Block a, [ Block a ])
extractIfBlock ((bif@(BlStatement a _ _ (StIfThen _ _ _)):bs)) = 
  let gbs = groupIf' bs 
      (conds, blocks, rest) = collectIfComponents (bif:gbs)
      lastBlock = last . last $ blocks
      span = getTransSpan bif lastBlock in
    (BlIf a span (getLabel bif) conds blocks, rest)
extractIfBlock _ = error "Head needs to be an IF-THEN statement"

collectIfComponents :: [ Block a ] -> ([ Maybe (Expression a) ], [ [ Block a ] ], [ Block a ])
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

collectStandardBlocks :: [ Block a ] -> ([ Block a ], [ Block a ])
collectStandardBlocks all@((BlStatement _ _ _ (StElsif _ _ e)):_) = ([], all)
collectStandardBlocks all@((BlStatement _ _ _ (StElse _ _)):_) = ([], all)
collectStandardBlocks all@(b@(BlStatement _ _ _ (StEndif _ _)):_) = ([ b ], all)
collectStandardBlocks (x:xs) = 
  let (bs, rest) = collectStandardBlocks xs in (x : bs, rest)
