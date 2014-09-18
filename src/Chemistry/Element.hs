module Chemistry.Element ( Element
                         , atomicNumber
                         , element
                         , elementBySymbol
                         , shellElectrons
                         , valanceElectrons
                         , covalentBounds ) where


data Element = Element { atomicNumber :: Int
                       , symbol :: String
                       , _name :: String
                       , _weight :: Double 
                       , _ionizationEnergy :: Double
                       , _electroNeg :: Double
                       , _electronConfig :: [Int]
                       } 
             | Unknown deriving (Eq, Show)
            
-- Periodic Table
-- Electron configuration is only provided for elements which are exception to Aufbau principle.             
ptable :: [Element]
ptable = [ Element 1 "H" "Hydrogen"     1.008       1.312   2.2     []
         , Element 2 "He" "Helium"      4.002602    2372.3  0       []
         , Element 3 "Li" "Lithium"     6.941       520.2   0.98    []
         , Element 4 "Be" "Beryllium"   9.012182    899.5   1.57    []
         , Element 5 "B" "Boron"        10.811      800.6   2.04    []
         , Element 6 "C" "Carbon"       12.011      1086.5  2.55    []
         , Element 7 "N" "Nitrogen"     14.007      1402.3  3.04    []
         , Element 8 "O" "Oxygen"       15.999      1313.9  3.44    []
         , Element 9 "F" "Fluorine"     18.9984032  1681    3.98    []
         , Element 10 "Ne" "Neon"       20.1797     2080.7  0       []
         , Element 11 "Na" "Sodium" 22.989768 0 0 []
         , Element 12 "Mg" "Magnesium" 24.305 0 0 []
         , Element 13 "Al" "Alluminium" 26.981 0 0 []
         , Element 14 "Si" "Silicon" 28.085 0 0 []
         , Element 15 "P" "Phosphous" 30.973 0 0 []
         , Element 16 "S" "Sulphur" 32.066 0 0 []
         , Element 17 "Cl" "Chlorine" 35.4527 0 0 []
         , Element 18 "Ar" "Argon" 39.948 0 0 []
         , Element 19 "K" "Potassium" 39.0983 0 0 []
         , Element 20 "Ca" "Calcium" 40.078 0 0 []
         , Element 21 "Sc" "Scandium" 44.95591 0 0 []
         , Element 22 "Ti" "Titanium" 47.88 0 0 []
         , Element 23 "V" "Vanadium" 50.9415 0 0 []
         , Element 24 "Cr" "Chromium" 51.9961 0 0 [2, 8, 13, 1]
         , Element 25 "Mn" "Manganese" 54.93805 0 0 []
         , Element 26 "Fe" "Iron" 55.845 0 0 []
         , Element 27 "Co" "Cobalt" 58.933 0 0 []
         , Element 28 "Ni" "Nickel" 58.6934 0 0 []
         , Element 29 "Cu" "Copper" 63.546 0 0 [2, 8, 18, 1]
         , Element 30 "Zn" "Zinc" 65.39 0 0 []
         , Element 31 "Ga" "Gallium" 69.723 0 0 []
         , Element 32 "Ge" "Germanium" 72.61 0 0 []
         , Element 33 "As" "Arsenic" 74.92159 0 0 []
         , Element 34 "Se" "Selenium" 78.96 0 0 []
         , Element 35 "Br" "Bromine" 79.904 0 0 []
         , Element 36 "Kr" "Krypton" 83.8 0 0 []
         , Element 37 "Rb" "Rubidium" 85.4678 0 0 []
         , Element 38 "Sr" "Strontium" 87.62 0 0 []
         , Element 39 "Y" "Yttrium" 88.90585 0 0 []
         , Element 40 "Zr" "Zirconium" 91.224 0 0 []
         , Element 41 "Nb" "Niobium" 92.90638 0 0 []
         , Element 42 "Mo" "Molybdenum" 95.94 0 0 []
         , Element 43 "Tc" "Technetium" 98.9063 0 0 []
         , Element 44 "Ru" "Ruthenium" 101.07 0 0 []
         , Element 45 "Rh" "Rhodium" 102.9055 0 0 []
         , Element 46 "Pd" "Palladium" 106.42 0 0 []
         , Element 47 "Ag" "Silver" 107.8682 0 0 []
         , Element 48 "Cd" "Cadmium" 112.411 0 0 []
         , Element 49 "In" "Indium" 114.82 0 0 []
         , Element 50 "Sn" "Tin" 118.71 0 0 []
         , Element 51 "Sb" "Antimony" 121.75 0 0 []
         , Element 52 "Te" "Tellurium" 127.6 0 0 []
         , Element 53 "I" "Iodine" 126.90447 0 0 []
         , Element 54 "Xe" "Xenon" 131.29 0 0 []
         , Element 55 "Cs" "Caesium" 132.90543 0 0 []
         , Element 56 "Ba" "Barium" 137.327 0 0 []
         , Element 57 "La" "Lanthanum" 138.9055 0 0 []
         , Element 58 "Ce" "Cerium" 140.115 0 0 []
         , Element 59 "Pr" "Praseodymium" 140.90765 0 0 []
         , Element 60 "Nd" "Neodymium" 144.24 0 0 []
         , Element 61 "Pm" "Promethium" 146.9151 0 0 []
         , Element 62 "Sm" "Samarium" 150.36 0 0 []
         , Element 63 "Eu" "Europium" 151.965 0 0 []
         , Element 64 "Gd" "Gadolinium" 157.25 0 0 []
         , Element 65 "Tb" "Terbium" 158.92534 0 0 []
         , Element 66 "Dy" "Dysprosium" 162.5 0 0 []
         , Element 67 "Ho" "Holmium" 164.93032 0 0 []
         , Element 68 "Er" "Erbium" 167.26 0 0 []
         , Element 69 "Tm" "Thulium" 168.93421 0 0 []
         , Element 70 "Yb" "Ytterbium" 173.04 0 0 []
         , Element 71 "Lu" "Lutetium" 174.967 0 0 []
         , Element 72 "Hf" "Hafnium" 178.49 0 0 []
         , Element 73 "Ta" "Tantalum" 180.9479 0 0 []
         , Element 74 "W" "Tungsten" 183.85 0 0 []
         , Element 75 "Re" "Rhenium" 186.207 0 0 []
         , Element 76 "Os" "Osmium" 190.2 0 0 []
         , Element 77 "Ir" "Iridium" 192.22 0 0 []
         , Element 78 "Pt" "Platinum" 195.08 0 0 []
         , Element 79 "Au" "Gold" 196.96654  0 0 []
         , Element 80 "Hg" "Mercury" 200.59  0 0 []
         , Element 81 "Tl" "Thallium" 204.3833 0 0 []
         , Element 82 "Pb" "Lead" 207.2 0 0 []
         , Element 83 "Bi" "Bismuth" 208.98037 0 0 []
         , Element 84 "Po" "Polonium" 208.9824 0 0 []
         , Element 85 "At" "Astatine" 209.9871 0 0 []
         , Element 86 "Rn" "Radon" 222.0176 0 0 []
         , Element 87 "Fr" "Francium" 223.0197 0 0 []
         , Element 88 "Ra" "Radium" 226.0254 0 0 []
         , Element 89 "Ac" "Actinium" 227.0278 0 0 []
         , Element 90 "Th" "Thorium" 232.0381 0 0 []
         , Element 91 "Pa" "Protactinium" 231.0359 0 0 []
         , Element 92 "U" "Uranium" 238.0289 0 0 []
         , Element 93 "Np" "Neptunium" 237.0482 0 0 []
         , Element 94 "Pu" "Plutonium" 244.0642 0 0 []
         , Element 95 "Am" "Americium" 243.0614 0 0 []
         , Element 96 "Cm" "Curium" 247.0703 0 0 []
         , Element 97 "Bk" "Berkelium" 247.0703 0 0 []
         , Element 98 "Cf" "Californium" 251.0796 0 0 []
         , Element 99 "Es" "Einsteinium" 252.0829 0 0 []
         , Element 100 "Fm" "Fermium" 257.0951 0 0 []
         , Element 101 "Md" "Mendelevium" 258.0986 0 0 []
         , Element 102 "No" "Nobelium" 259.1009 0 0 []
         , Element 103 "Lr" "Lawrencium" 260.1053 0 0 []
         , Element 104 "Rf" "Rutherfordium" 261.1087 0 0 []
         , Element 105 "Db" "Dubnium" 262.1138 0 0 []
         , Element 106 "Sg" "Seaborgium" 263.1182 0 0 []
         , Element 107 "Bh" "Bohrium" 262.1229 0 0 []
         , Element 108 "Hs" "Hassium" 265 0 0 []
         , Element 109 "Mt" "Meitnerium" 266 0 0 []
         , Element 110 "Ds" "Darmstadtium" 269 0 0 []
         , Element 111 "Rg" "Roentgenium" 272 0 0 []
         , Element 112 "Cn" "Copernicium" 285 0 0 []
         , Element 113 "Uut" "Ununtrium" 284 0 0 []
         , Element 114 "Uug" "Ununquadium" 289 0 0 []
         , Element 115 "Uup" "Ununpentium" 288 0 0 []
         , Element 116 "Uuh" "Ununhexium" 293 0 0 []
         , Element 117 "Uus" "Ununseptium" 294  0 0 []
         , Element 118 "Uuo" "Ununoctium" 294 0 0 []]
           

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

-- | Show number of electrons in each shell
--
-- > let e = element 8
-- > shellElectrons e == [2, 6] 
shellElectrons :: Element -> [Int]
shellElectrons (Element _ _ _ _ _ _ (x:xs)) = x:xs
shellElectrons e = filter (> 0) $ f (fillShells (atomicNumber e)) 
        where f :: [(Int, Int, Int)] -> [Int]
              f ss = [sum (g n ss) | n <- [1..m]]
                where m = length ss
                      g l = map (\(a,_,c) -> if a == l then c else 0)
                      
-- | Number of valance electrons
--
-- > let e = element 8
-- > valanceElectrons e == 6 
valanceElectrons :: Element -> Int
valanceElectrons e = last (shellElectrons e)

-- | Number of covalent bounds in element
--
-- > let e = element 8
-- > covalentBounds e == 2 
covalentBounds :: Element -> Int
covalentBounds e = min n (8-n)
    where n = valanceElectrons e


-- How many electrons are in each subshell
subshellMaxElectrons :: [Int]
subshellMaxElectrons = [2, 6, 10, 14, 18]

-- Generate possible config for given maximum number of shells.
-- The order is based on Aufbau principle. 
-- http://en.wikipedia.org/wiki/Aufbau_principle
shellConfigGen :: Int -> [(Int, Int)]
shellConfigGen n = [(i,m-i) | m <- [2..n+n], i <- [((m+1) `div` 2)..m-1] ] 

-- Fill shells with given number of electrons
fillShells :: Int -> [( Int  -- Shell number
                    , Int  -- Shell type
                    , Int  -- #Electrons
                      )]
fillShells = f (shellConfigGen 5)
        where f :: [(Int, Int)] -> Int -> [(Int, Int, Int)]
              f [] _ = []
              f ((i,j):xs) m | m == 0 = []
                             | m < l = [(i, j, m)]
                             | otherwise = (i, j, l) : f xs (m - l)
                             where l = subshellMaxElectrons !! (j-1)  
                                   
                 

              
              