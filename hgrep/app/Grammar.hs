module Grammar where

data GramChar = GCLit Char | GCLine GLine | GCBrOpen GBrOpen | GCBrClose GBrClose | GCStar GStar | GCEpsilon GEpsilon
    deriving (Eq, Show)

data GLit = GLitC Char
    deriving (Eq, Show)
data GLine = GLineC
    deriving (Eq, Show)
data GBrOpen = GBrOpenC
    deriving (Eq, Show)
data GBrClose = GBrCloseC
    deriving (Eq, Show)
data GEpsilon = GEpsilonC
    deriving (Eq, Show)
data GStar = GStarC
    deriving (Eq, Show)

data Re0 = Re0One Re1 | Re0Mult Re1 GLine Re0
    deriving (Eq, Show)
data Re1 = Re1One Re2 | Re1Mult Re2 Re1
    deriving (Eq, Show)
data Re2 = Re2Post Atom Postfix
    deriving (Eq, Show)
data Postfix = PostfixStar GStar | PostfixEpsilon
    deriving (Eq, Show)
data Atom = AtomLit GLit | AtomBr GBrOpen Re0 GBrClose | AtomEpsilon GEpsilon
    deriving (Eq, Show)
data RegEx = RegExEmpty | RegExRe0 Re0 | RegExRe1 Re1 | RegExRe2 Re2 | RegExAtom Atom
    deriving (Eq, Show)

nullable :: RegEx -> Bool
nullable RegExEmpty = False
nullable (RegExRe0 (Re0One re1)) = nullable (RegExRe1 re1)
nullable (RegExRe0 (Re0Mult re1 gline re0)) = nullable (RegExRe1 re1) || nullable (RegExRe0 re0)
nullable (RegExRe1 (Re1One re2)) = nullable (RegExRe2 re2)
nullable (RegExRe1 (Re1Mult re2 re1)) = nullable (RegExRe2 re2) && nullable (RegExRe1 re1)
nullable (RegExRe2 (Re2Post atom (PostfixStar gstar))) = True
nullable (RegExRe2 (Re2Post atom (PostfixEpsilon))) = nullable (RegExAtom atom)
nullable (RegExAtom (AtomEpsilon _)) = True
nullable (RegExAtom (AtomLit (GLitC c))) = False
nullable (RegExAtom (AtomBr bro re0 brc)) = nullable (RegExRe0 re0)

regex2re0 :: RegEx -> Re0
regex2re0 (RegExRe0 re0) = re0
regex2re0 (RegExRe1 re1) = (Re0One re1)
regex2re0 (RegExRe2 re2) = regex2re0 (RegExRe1 (Re1One re2))
regex2re0 (RegExAtom (AtomBr bro re0 brc)) = re0
regex2re0 (RegExAtom atom) = regex2re0 (RegExRe2 (Re2Post atom PostfixEpsilon))
regex2re0 _ = error "Cannot convert improper RegEx to Re0"

regex2re1 :: RegEx -> Re1
regex2re1 (RegExRe1 re1) = re1
regex2re1 (RegExRe2 re2) = (Re1One re2)
regex2re1 (RegExAtom atom) = regex2re1 (RegExRe2 (Re2Post atom PostfixEpsilon))
regex2re1 (RegExRe0 re0) = regex2re1 (RegExAtom (AtomBr GBrOpenC re0 GBrCloseC))
regex2re1 _ = error "Cannot convert improper RegEx to Re1"

regex2re2 :: RegEx -> Re2
regex2re2 (RegExRe2 re2) = re2
regex2re2 (RegExAtom atom) = regex2re2 (RegExRe2 (Re2Post atom PostfixEpsilon))
regex2re2 (RegExRe0 re0) = regex2re2 (RegExAtom (AtomBr GBrOpenC re0 GBrCloseC))
regex2re2 (RegExRe1 re1) = regex2re2 (RegExRe0 (Re0One re1))
regex2re2 _ = error "Cannot convert improper RegEx to Re2"

derivative :: Char -> RegEx -> RegEx
derivative _ RegExEmpty = RegExEmpty
derivative c (RegExRe0 (Re0One re1)) = derivative c (RegExRe1 re1)
derivative c (RegExRe0 (Re0Mult re1 gline re0)) = if derivative c (RegExRe1 re1) == RegExEmpty then derivative c (RegExRe0 re0) else (
    if derivative c (RegExRe0 re0) == RegExEmpty then derivative c (RegExRe1 re1) else 
        RegExRe0 (Re0Mult (regex2re1 (derivative c (RegExRe1 re1))) GLineC (regex2re0 (derivative c (RegExRe0 re0))))
    )
derivative c (RegExRe1 (Re1One re2)) = derivative c (RegExRe2 re2)
derivative c (RegExRe1 (Re1Mult re2 re1))
    | nullable (RegExRe2 re2) = if (derivative c (RegExRe2 re2)) == RegExEmpty then derivative c (RegExRe1 re1) else (
          if derivative c (RegExRe1 re1) == RegExEmpty then RegExRe1 (Re1Mult (regex2re2 (derivative c (RegExRe2 re2))) re1) else
              RegExRe0 (Re0Mult (Re1Mult (regex2re2 (derivative c (RegExRe2 re2))) re1) GLineC (regex2re0 (derivative c (RegExRe1 re1))))
      )
    | otherwise               =          RegExRe1 (Re1Mult (regex2re2 (derivative c (RegExRe2 re2))) re1)
derivative c (RegExRe2 (Re2Post atom (PostfixStar gstar))) = if derivative c (RegExAtom atom) == RegExEmpty then RegExRe2 (Re2Post atom (PostfixStar GStarC)) else 
        RegExRe1 (Re1Mult (regex2re2 (derivative c (RegExAtom atom))) (regex2re1 (RegExRe2 (Re2Post atom (PostfixStar GStarC)))))
derivative c (RegExRe2 (Re2Post atom PostfixEpsilon)) = derivative c (RegExAtom atom)
derivative _ (RegExAtom (AtomEpsilon _)) = RegExEmpty
derivative c (RegExAtom (AtomLit (GLitC c')))
    | c == c'   = (RegExAtom (AtomEpsilon GEpsilonC))
    | otherwise = RegExEmpty

match :: String -> RegEx -> Bool
match [] r = nullable r
match (c:cs) r = match cs (derivative c r)

