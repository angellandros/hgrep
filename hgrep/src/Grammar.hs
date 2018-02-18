module Grammar where

import Data.List.Utils

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

regex2atom :: RegEx -> Atom
regex2atom (RegExAtom atom) = atom
regex2atom r = AtomBr GBrOpenC (regex2re0 r) GBrCloseC

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
    | otherwise               = if derivative c (RegExRe2 re2) == RegExEmpty then RegExEmpty else
        RegExRe1 (Re1Mult (regex2re2 (derivative c (RegExRe2 re2))) re1)
derivative c (RegExRe2 (Re2Post atom (PostfixStar gstar))) = if derivative c (RegExAtom atom) == RegExEmpty then RegExEmpty else 
        RegExRe1 (Re1Mult (regex2re2 (derivative c (RegExAtom atom))) (regex2re1 (RegExRe2 (Re2Post atom (PostfixStar GStarC)))))
derivative c (RegExRe2 (Re2Post atom PostfixEpsilon)) = derivative c (RegExAtom atom)
derivative _ (RegExAtom (AtomEpsilon _)) = RegExEmpty
derivative c (RegExAtom (AtomLit (GLitC c')))
    | comChar c' c = (RegExAtom (AtomEpsilon GEpsilonC))
    | otherwise    = RegExEmpty
derivative c (RegExAtom (AtomBr _ re0 _)) = derivative c (RegExRe0 re0)

-- comparing characters (especially escaped ones)
comChar :: Char -> Char -> Bool
comChar '‹' '(' = True
comChar '›' ')' = True
comChar '‖' '|' = True
comChar '×' '*' = True
comChar '.' _   = True
comChar c c' = if c == c' then True else False

-- return whether a String match a RegEx
match :: String -> RegEx -> Bool
match [] r = nullable r
match (c:cs) r = match cs (derivative c r)

-- utility function : merge a String with the first element of a String List
mergeFirst :: String -> [String] -> [String]
mergeFirst s' [] = [s']
mergeFirst s' (s:ss) = (s'++s):ss

-- split first occurence of a Char in a String, in the top level 0
-- with n /= 0 nothing will match
splitWithLevel :: String -> Char -> Int -> [String]
splitWithLevel [] _ _ = [""]
splitWithLevel (c:cs) c' n
    | c == '(' = mergeFirst [c] (splitWithLevel cs c' (n + 1))
    | c == ')' = mergeFirst [c] (splitWithLevel cs c' (n - 1))
splitWithLevel (c:cs) c' 0
    | c == c'   = ["", cs]
    | otherwise = mergeFirst [c] (splitWithLevel cs c' 0)
splitWithLevel (c:cs) c' n = mergeFirst [c] (splitWithLevel cs c' n)

-- String to RegEx translation function
middotize :: String -> String
middotize [] = []
middotize [c] = [c]
middotize (c:cs) = [c, '·'] ++ middotize cs

dotize :: String -> String
dotize s = 
    replace "··" "·" $
    replace "·*" "*" $
    replace "·|·" "|" $
    replace "·(·" "(" $
    replace "·)·" ")" $
    replace "(·" "(" $
    replace "·)" ")" $
    middotize $
    -- escape characters
    replace "\\*" "×" $
    replace "\\|" "‖" $
    replace "\\)" "›" $
    replace "\\(" "‹" $
    -- support ranges : a-z A-Z 0-9
    replace "[" "(" $
    replace "]" ")" $
    replace "a-z" "a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z" $
    replace "A-Z" "A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z" $
    replace "0-9" "0|1|2|3|4|5|6|7|8|9" $
    replace "[|" "[" $
    replace "|]" "]" $
    replace "||" "|" $
    replace "a-z" "|a-z|" $
    replace "A-Z" "|A-Z|" $
    replace "0-9" "|0-9|" $
        s

str2regex :: String -> RegEx
str2regex s = let s' = dotize s in 
    case splitWithLevel s' '|' 0 of 
        [s1, s2] -> RegExRe0 (Re0Mult (regex2re1 (str2regex s1)) GLineC (regex2re0 (str2regex s2))) 
        _        -> case splitWithLevel s' '·' 0 of
            [s1, s2] -> RegExRe1 (Re1Mult (regex2re2 (str2regex s1)) (regex2re1 (str2regex s2)))
            _        -> case splitWithLevel s' '*' 0 of
                [s1, ""]  -> RegExRe2 (Re2Post (regex2atom (str2regex s1)) (PostfixStar GStarC))
                _         -> case (take 1 s', drop (length s' - 1) s') of
                    ("(", ")") -> RegExAtom (AtomBr GBrOpenC (regex2re0 (str2regex (drop 1 (take (length s' - 1) s')))) GBrCloseC)
                    _          -> case s' of
                        []       -> RegExAtom (AtomEpsilon GEpsilonC)
                        [c]      -> RegExAtom (AtomLit (GLitC c))
                        _        -> RegExEmpty
