{- |
Module Radium.Element
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains Periodic Table with information about all known elements.
-}

module Radium.Element ( Element(..)
                      , Ion (..)
                      , OrbitalId (..)
                      , OrbitalState (..)
                      , covalentBounds
                      , electronConfig
                      , element
                      , elementBySymbol
                      , ionElectronConfig
                      , possibleElectronConfigs
                      , possibleIonElectronConfigs
                      , possibleIonValences
                      , possibleValences
                      , valenceElectrons
                      ) where

import           Control.Arrow ((&&&))
import           Data.Foldable (toList)
import           Data.List     (findIndex, findIndices, unfoldr)
import qualified Data.Map      as Map
import           Data.Maybe    (listToMaybe)
import           Data.Sequence (fromList, update)


-- | Orbitals have names: s, p, d ,f, g, h
data OrbitalId = S | P | D | F | G | H
  deriving (Show, Eq, Enum, Ord)


-- | Element from periodic table configuration
data Element = Element { atomicNumber      :: Int
                       , symbol            :: String
                       , _name             :: String
                       , atomWeight        :: Double
                       , electroNegativity :: Double
                       , ionizationEnergy  :: Double
                       }
             | Unknown deriving (Eq)
instance Show Element where
  show Unknown              = "*"
  show Element { symbol=s } = s


-- | Atom with non-ground atom shell
data Ion = Ion {
  ionElement :: Element,
  ionCharge  :: Int
}
instance Show Ion where
  show (Ion e n) | n ==  0 = show e
                 | n ==  1 = show e ++ "+"
                 | n >   1 = show e ++ "+" ++ show n
                 | n == -1 = show e ++ "-"
                 | n <  -1 = show e ++ "-" ++ show n
  show _ = "Unknown ion"

-- | Orbital configuration
data OrbitalState = State {
  orbitalLayer     :: Int,
  orbitalSublayer  :: OrbitalId,
  orbitalElectrons :: Int
} deriving (Show, Eq)

-- Periodic Table
ptable :: [Element]
ptable = [ Element 1 "H" "Hydrogen"         1.008       2.2     1.312
         , Element 2 "He" "Helium"          4.002602    0       2372.3
         , Element 3 "Li" "Lithium"         6.941       0.98    520.2
         , Element 4 "Be" "Beryllium"       9.012182    1.57    899.5
         , Element 5 "B" "Boron"            10.811      2.04    800.6
         , Element 6 "C" "Carbon"           12.011      2.55    1086.5
         , Element 7 "N" "Nitrogen"         14.007      3.04    1402.3
         , Element 8 "O" "Oxygen"           15.999      3.44    1313.9
         , Element 9 "F" "Fluorine"         18.9984032  3.98    1681
         , Element 10 "Ne" "Neon"           20.1797     0       2080.7
         , Element 11 "Na" "Sodium"         22.989768   0.93    495.8
         , Element 12 "Mg" "Magnesium"      24.305      1.31    737.7
         , Element 13 "Al" "Alluminium"     26.981      1.61    577.5
         , Element 14 "Si" "Silicon"        28.085      1.9     786.5
         , Element 15 "P" "Phosphous"       30.973      2.19    1011.8
         , Element 16 "S" "Sulphur"         32.066      2.58    999.6
         , Element 17 "Cl" "Chlorine"       35.4527     3.16    1251.2
         , Element 18 "Ar" "Argon"          39.948      0       1520.6
         , Element 19 "K" "Potassium"       39.0983     0.82    418.8
         , Element 20 "Ca" "Calcium"        40.078      1       589.8
         , Element 21 "Sc" "Scandium"       44.95591    1.36    633.1
         , Element 22 "Ti" "Titanium"       47.88       1.54    658.8
         , Element 23 "V" "Vanadium"        50.9415     1.63    650.9
         , Element 24 "Cr" "Chromium"       51.9961     1.66    652.9
         , Element 25 "Mn" "Manganese"      54.93805    1.55    717.3
         , Element 26 "Fe" "Iron"           55.845      1.83    762.5
         , Element 27 "Co" "Cobalt"         58.933      1.88    760.4
         , Element 28 "Ni" "Nickel"         58.6934     1.91    737.1
         , Element 29 "Cu" "Copper"         63.546      1.9     745.5
         , Element 30 "Zn" "Zinc"           65.39       1.65    906.4
         , Element 31 "Ga" "Gallium"        69.723      1.81    578.8
         , Element 32 "Ge" "Germanium"      72.61       2.01    762
         , Element 33 "As" "Arsenic"        74.92159    2.18    947
         , Element 34 "Se" "Selenium"       78.96       2.55    941
         , Element 35 "Br" "Bromine"        79.904      2.96    1139.9
         , Element 36 "Kr" "Krypton"        83.8        3       1350.8
         , Element 37 "Rb" "Rubidium"       85.4678     0.82    403
         , Element 38 "Sr" "Strontium"      87.62       0.95    549.5
         , Element 39 "Y" "Yttrium"         88.90585    1.22    600
         , Element 40 "Zr" "Zirconium"      91.224      1.33    640.1
         , Element 41 "Nb" "Niobium"        92.90638    1.6     652.1
         , Element 42 "Mo" "Molybdenum"     95.94       2.16    684.3
         , Element 43 "Tc" "Technetium"     98.9063     1.9     702
         , Element 44 "Ru" "Ruthenium"      101.07      2.2     710.2
         , Element 45 "Rh" "Rhodium"        102.9055    2.28    719.7
         , Element 46 "Pd" "Palladium"      106.42      2.2     804.4
         , Element 47 "Ag" "Silver"         107.8682    1.93    731
         , Element 48 "Cd" "Cadmium"        112.411     1.69    867.8
         , Element 49 "In" "Indium"         114.82      1.78    558.3
         , Element 50 "Sn" "Tin"            118.71      1.96    708.6
         , Element 51 "Sb" "Antimony"       121.75      2.05    834
         , Element 52 "Te" "Tellurium"      127.6       2.1     869.3
         , Element 53 "I" "Iodine"          126.90447   2.66    1008.4
         , Element 54 "Xe" "Xenon"          131.29      2.6     1170.4
         , Element 55 "Cs" "Caesium"        132.90543   0.79    375.7
         , Element 56 "Ba" "Barium"         137.327     0.89    502.9
         , Element 57 "La" "Lanthanum"      138.9055    1.1     538.1
         , Element 58 "Ce" "Cerium"         140.115     1.12    534.4
         , Element 59 "Pr" "Praseodymium"   140.90765   1.13    527
         , Element 60 "Nd" "Neodymium"      144.24      1.14    533.1
         , Element 61 "Pm" "Promethium"     146.9151    0       540
         , Element 62 "Sm" "Samarium"       150.36      1.17    544.5
         , Element 63 "Eu" "Europium"       151.965     0       547.1
         , Element 64 "Gd" "Gadolinium"     157.25      1.2     593.4
         , Element 65 "Tb" "Terbium"        158.92534   0       565.8
         , Element 66 "Dy" "Dysprosium"     162.5       1.22    573
         , Element 67 "Ho" "Holmium"        164.93032   1.23    581
         , Element 68 "Er" "Erbium"         167.26      1.24    589.3
         , Element 69 "Tm" "Thulium"        168.93421   1.25    596.7
         , Element 70 "Yb" "Ytterbium"      173.04      0       603.4
         , Element 71 "Lu" "Lutetium"       174.967     1.27    523.5
         , Element 72 "Hf" "Hafnium"        178.49      1.3     658.5
         , Element 73 "Ta" "Tantalum"       180.9479    1.5     761
         , Element 74 "W" "Tungsten"        183.85      2.36    770
         , Element 75 "Re" "Rhenium"        186.207     1.9     760
         , Element 76 "Os" "Osmium"         190.2       2.2     840
         , Element 77 "Ir" "Iridium"        192.22      2.2     880
         , Element 78 "Pt" "Platinum"       195.08      2.28    870
         , Element 79 "Au" "Gold"           196.96654   2.54    890.1
         , Element 80 "Hg" "Mercury"        200.59      2       1007.1
         , Element 81 "Tl" "Thallium"       204.3833    1.62    589.4
         , Element 82 "Pb" "Lead"           207.2       2.33    715.6
         , Element 83 "Bi" "Bismuth"        208.98037   2.02    703
         , Element 84 "Po" "Polonium"       208.9824    2       812.1
         , Element 85 "At" "Astatine"       209.9871    2.2     920
         , Element 86 "Rn" "Radon"          222.0176    0       1037
         , Element 87 "Fr" "Francium"       223.0197    0.7     380
         , Element 88 "Ra" "Radium"         226.0254    0.9     509.3
         , Element 89 "Ac" "Actinium"       227.0278    1.1     499
         , Element 90 "Th" "Thorium"        232.0381    1.3     587
         , Element 91 "Pa" "Protactinium"   231.0359    1.5     568
         , Element 92 "U" "Uranium"         238.0289    1.38    597.6
         , Element 93 "Np" "Neptunium"      237.0482    1.36    604.5
         , Element 94 "Pu" "Plutonium"      244.0642    1.28    584.7
         , Element 95 "Am" "Americium"      243.0614    1.3     578
         , Element 96 "Cm" "Curium"         247.0703    1.3     581
         , Element 97 "Bk" "Berkelium"      247.0703    1.3     601
         , Element 98 "Cf" "Californium"    251.0796    1.3     608
         , Element 99 "Es" "Einsteinium"    252.0829    1.3     619
         , Element 100 "Fm" "Fermium"       257.0951    1.3     627
         , Element 101 "Md" "Mendelevium"   258.0986    1.3     635
         , Element 102 "No" "Nobelium"      259.1009    1.3     642
         , Element 103 "Lr" "Lawrencium"    260.1053    0       0
         , Element 104 "Rf" "Rutherfordium" 261.1087    0       0
         , Element 105 "Db" "Dubnium"       262.1138    0       0
         , Element 106 "Sg" "Seaborgium"    263.1182    0       0
         , Element 107 "Bh" "Bohrium"       262.1229    0       0
         , Element 108 "Hs" "Hassium"       265         0       0
         , Element 109 "Mt" "Meitnerium"    266         0       0
         , Element 110 "Ds" "Darmstadtium"  269         0       0
         , Element 111 "Rg" "Roentgenium"   272         0       0
         , Element 112 "Cn" "Copernicium"   285         0       0
         , Element 113 "Nh" "Nihonium"      284         0       0
         , Element 114 "Fl" "Flerovium"     289         0       0
         , Element 115 "Mc" "Moscovium"     288         0       0
         , Element 116 "Lv" "Livermorium"   293         0       0
         , Element 117 "Ts" "Tennessine"    294         0       0
         , Element 118 "Og" "Oganesson"     294         0       0
         ]


-- | Find element by its atomic number
--
-- > atomicNumber (element 8) == 8
element :: Int -> Element
element n = f n ptable
    where f :: Int -> [Element] -> Element
          f _ [] = Unknown
          f x (e:es) | atomicNumber e == x = e
                     | otherwise = f x es


-- | Find element by its symbol
--
-- > atomicNumber (elementBySymbol "O") == 8
elementBySymbol :: String -> Element
elementBySymbol ns = f ns ptable
    where f :: String -> [Element] -> Element
          f _ [] = Unknown
          f xs (e:es) | symbol e == xs = e
                      | otherwise = f xs es

-- Electron configuration exceptions to Aufbau principle
configExceptions :: Map.Map Int [Int]
configExceptions = Map.fromList [ (24, [2, 8, 13, 1])
                                , (29, [2, 8, 18, 1])
                                , (41, [2, 8, 18, 12, 1])
                                , (42, [2, 8, 18, 13, 1])
                                , (44, [2, 8, 18, 15, 1])
                                , (45, [2, 8, 18, 16, 1])
                                , (46, [2, 8, 18, 18])
                                , (47, [2, 8, 18, 18, 1])
                                , (57, [2, 8, 18, 18, 9, 2])
                                , (58, [2, 8, 18, 19, 9, 2])
                                , (64, [2, 8, 18, 25, 9, 2])
                                , (79, [2, 8, 18, 32, 18, 1])
                                , (89, [2, 8, 18, 32, 18, 9, 2])
                                , (90, [2, 8, 18, 32, 18, 10, 2])
                                , (91, [2, 8, 18, 32, 20, 9, 2])
                                , (92, [2, 8, 18, 32, 21, 9, 2])
                                , (93, [2, 8, 18, 32, 22, 9, 2])
                                , (96, [2, 8, 18, 32, 25, 9, 2]) ]

-- | Show number of electrons in each shell
--   For elements which are exception to Aufbau principle configuration is given manually
--   Is it possible to calculate it for all elements based only on atom properties?
--
-- > let e = element 8
-- > electronConfig e == [2, 6]
electronConfig :: Element -> [Int]
electronConfig e = case Map.lookup (atomicNumber e) configExceptions of
                    Just val -> val
                    _ -> filter (> 0) $ f (fillShells (atomicNumber e))
                        where f :: [OrbitalState] -> [Int]
                              f ss = [sum (g n ss) | n <- [1..m]]
                                where m = length ss
                                      g l = map (\(State a _ c) -> if a == l then c else 0)


-- | Show number of electrons in each shell for an ionized state of an atom
--
-- > let i = Ion (element 20) 2
-- > ionElectronConfig i == [2, 8, 8]
-- > let i = Ion (element 17) (-1)
-- > ionElectronConfig i == [2, 8, 8]
ionElectronConfig :: Ion -> [Int]
ionElectronConfig (Ion e c) | c > 0 = prt . helperP c    $ electronConfig e
                            | c < 0 = prt . helperN (-c) $ electronConfig e
                            | otherwise = electronConfig e
  where
    prt [] = []
    prt l  = if last l == 0 then prt $ init l else l

    helperP ch conf | ch == 0 = conf
                    | last conf > 0 = helperP (ch - 1) (init conf ++ [last conf - 1])
                    | otherwise = helperP ch $ init conf
    helperN ch conf | ch == 0 = conf
                    | last conf < (layerOrbitals !! (length conf - 1) * 2) = helperN (ch - 1) (init conf ++ [last conf + 1])
                    | otherwise = helperN ch $ conf ++ [0]

-- | Number of valance electrons
--
-- > let e = element 8
-- > valenceElectrons e == 6
valenceElectrons :: Element -> Int
valenceElectrons e = last (electronConfig e)

-- | Number of covalent bounds in element
--
-- > let e = element 8
-- > covalentBounds e == 2
covalentBounds :: Element -> Int
covalentBounds e = min n (8 - n)
  where n = valenceElectrons e


-- | Get list of all possible coordination numbers (valences)
possibleValences :: Element -> [Int]
possibleValences el = valenceHelper <$> possibleElectronConfigs el

-- | Get list of all possible valences for an ion
possibleIonValences :: Ion -> [Int]
possibleIonValences i = valenceHelper <$> possibleIonElectronConfigs i

valenceHelper :: [OrbitalState] -> Int
valenceHelper = foldr val 0
  where valSub s e = if e < subshellOrbital s then e else subshellOrbital s * 2 - e
        val (State _ s e) b = b + valSub s e


-- s sub-shell consists of only 1 orbital.
-- p sub-shell consists of 3 orbitals.
-- d sub-shell consists of 5 orbitals.
-- f sub-shell consists of 7 orbitals.
-- g sub-shell consists of 9 orbitals.
subshellOrbitals :: [Int]
subshellOrbitals = [1, 3, 5, 7, 9]

-- Get concrete number of orbitals by id
subshellOrbital :: OrbitalId -> Int
subshellOrbital = (subshellOrbitals !!) . fromEnum

layerOrbitals :: [Int]
layerOrbitals = scanl1 (+) subshellOrbitals


-- Turn atom into the next excited state
excite :: [OrbitalState] -> Maybe [OrbitalState]
excite conf = fmap removeEmptyShells excConf
  where
    highestPaired :: [OrbitalState] -> Maybe Int
    highestPaired = listToMaybe . reverse . findIndices (\(State _ s e) -> subshellOrbital s < e)

    lowestFree :: [OrbitalState] -> Maybe Int
    lowestFree = findIndex (\(State _ s e) -> subshellOrbital s > e)

    increase (State l s e) = State l s (e + 1)
    decrease (State l s e) = State l s (e - 1)

    expConf = appendEmptyShells conf

    excConf = case (highestPaired &&& lowestFree) expConf of
      (Nothing, _)     -> Nothing
      (_, Nothing)     -> Nothing
      (Just h, Just l) -> if orbitalLayer (conf !! h) == orbitalLayer (expConf !! l)
                          then
                            Just
                              . toList
                              . update l (increase $ expConf !! l)
                              . update h (decrease $ expConf !! h)
                              $ fromList expConf
                          else
                            Nothing


possibleOrbitalStates :: [OrbitalState] -> [[OrbitalState]]
possibleOrbitalStates s = s : unfoldr helper s
  where helper x = do
          res <- excite x
          return (res, res)

-- | Generate all possible electron configs (unexcited and excited) for a given element
possibleElectronConfigs :: Element -> [[OrbitalState]]
possibleElectronConfigs Element { atomicNumber=e } = possibleOrbitalStates $ fillShells e
possibleElectronConfigs Unknown = []

-- remove electrons from the highest layer (useful for ionization)
-- if it is impossible to take given number of electrons, returns Nothing
removeHighest :: Int -> [OrbitalState] -> Maybe [OrbitalState]
removeHighest 0 s = return s
removeHighest n s = do
  True <- return . not $ null s
  let hLayer = maximum $ fmap orbitalLayer s

  let sublayersIdx = findIndices ((==hLayer) . orbitalLayer) s
  True <- return . not $ null sublayersIdx

  let curIdx = maximum sublayersIdx
  let curSublayer = s !! curIdx
  let newSublayer = curSublayer { orbitalElectrons=orbitalElectrons curSublayer - 1 }
  let newState = case orbitalElectrons newSublayer of
        0 -> filter (/= curSublayer) s
        _ -> toList . update curIdx newSublayer $ fromList s
  removeHighest (n-1) newState

-- | Generate all possible electron configs (unexcited and excited) for a given ion
-- | By now it assumes that removing electrons starts from the highest layer,
-- | whereas adding electrons follows the same way as while moving along periodic table
possibleIonElectronConfigs :: Ion -> [[OrbitalState]]
possibleIonElectronConfigs (Ion e c) | c < 0 = possibleElectronConfigs $ element (atomicNumber e - c)
                                     | c > 0 = maybe [] possibleOrbitalStates (removeHighest c . fillShells $ atomicNumber e)
                                     | otherwise = possibleElectronConfigs e

-- Generate possible config for a given subshell id.
-- The order is based on Aufbau principle.
-- http://en.wikipedia.org/wiki/Aufbau_principle
-- Return ordered list of shell number with subshell id.
shellConfigGen :: OrbitalId -> [(Int, OrbitalId)]
shellConfigGen oid = [(i, toEnum (m - i - 1)) |
                      m <- [2 .. n + n],
                      i <- [((m + 1) `div` 2) .. m - 1] ]
  where n = fromEnum oid


-- Appends sublayers which don't carry any electron in the ground state
-- but could carry in excited state
appendEmptyShells :: [OrbitalState] -> [OrbitalState]
appendEmptyShells conf = conf ++ addSublayers
  where maxLayer = maximum (fmap orbitalLayer conf)
        maxSublayer = fromEnum $ maximum (orbitalSublayer <$> filter ((==maxLayer) . orbitalLayer) conf)
        addSublayers = fmap (\x -> State maxLayer (toEnum x) 0) [(maxSublayer + 1) .. maxLayer - 1]


-- Do the opposite of the function above
removeEmptyShells :: [OrbitalState] -> [OrbitalState]
removeEmptyShells = filter ((/= 0) . orbitalElectrons)


-- Fill shells up to given number of electrons.
-- This function only fills subshells up to 'g'
fillShells :: Int -> [OrbitalState]
fillShells = f (shellConfigGen G)
    where f :: [(Int, OrbitalId)] -> Int -> [OrbitalState]
          f [] _ = []
          f ((i,j) : xs) m | m == 0 = []
                           | m <= l = [State i j m]
                           | otherwise = State i j l : f xs (m - l)
              where
                  l = subshellOrbital j * 2
