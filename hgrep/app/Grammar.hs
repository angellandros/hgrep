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
data Atom = AtomLit GLit | AtomBr GBrOpen Re0 GBrClose
    deriving (Eq, Show)
data RegEx = RegExEmpty | RegExEpsilon | RegExRe0 Re0 | RegExRe1 Re1 | RegExRe2 Re2 | RegExAtom Atom
    deriving (Eq, Show)

nullable :: RegEx -> Bool
nullable RegExEmpty = False
nullable RegExEpsilon = True
nullable (RegExRe0 (Re0One re1)) = nullable (RegExRe1 re1)
nullable (RegExRe0 (Re0Mult re1 gline re0)) = nullable (RegExRe1 re1) || nullable (RegExRe0 re0)
nullable (RegExRe1 (Re1One re2)) = nullable (RegExRe2 re2)
nullable (RegExRe1 (Re1Mult re2 re1)) = nullable (RegExRe2 re2) && nullable (RegExRe1 re1)
nullable (RegExRe2 (Re2Post atom (PostfixStar gstar))) = True
nullable (RegExRe2 (Re2Post atom (PostfixEpsilon))) = nullable (RegExAtom atom)
nullable (RegExAtom (AtomLit (GLitC c))) = False
nullable (RegExAtom (AtomBr bro re0 brc)) = nullable (RegExRe0 re0)


