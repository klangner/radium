module Chemistry.Element ( Element
                       , atomicNumber
                       , element
                       , elementBySymbol
                       , shellElectrons
                       , valanceElectrons
                       , covalentBounds 
                         ) where


data Element = Element { atomicNumber :: Int
                       , symbol :: String
                       , _name :: String
                       , _weight :: Double 
                       } deriving Show
            
ptable :: [Element]
ptable = [ Element 1 "H" "Hydrogen" 1.008 
         , Element 2 "He" "Helium" 4.002602 
         , Element 3 "Li" "Lithium" 6.941 
         , Element 4 "Be" "Beryllium" 9.012182 
         , Element 5 "B" "Boron" 10.811 
         , Element 6 "C" "Carbon" 12.011 
         , Element 7 "N" "Nitrogen" 14.007 
         , Element 8 "O" "Oxygen" 15.999
         , Element 9 "F" "Fluorine" 18.9984032 
         , Element 10 "Ne" "Neon" 20.1797 
         , Element 11 "Na" "Sodium" 22.989768 
         , Element 12 "Mg" "Magnesium" 24.305 
         , Element 13 "Al" "Alluminium" 26.981
         , Element 14 "Si" "Silicon" 28.085
         , Element 15 "P" "Phosphous" 30.973
         , Element 16 "S" "Sulphur" 32.066 
         , Element 17 "Cl" "Chlorine" 35.4527 
         , Element 18 "Ar" "Argon" 39.948 
         , Element 19 "K" "Potassium" 39.0983 
         , Element 20 "Ca" "Calcium" 40.078 
         , Element 21 "Sc" "Scandium" 44.95591 
         , Element 22 "Ti" "Titanium" 47.88 
         , Element 23 "V" "Vanadium" 50.9415 
         , Element 24 "Cr" "Chromium" 51.9961 
         , Element 25 "Mn" "Manganese" 54.93805 
         , Element 26 "Fe" "Iron" 55.845
         , Element 27 "Co" "Cobalt" 58.933
         , Element 28 "Ni" "Nickel" 58.6934
         , Element 29 "Cu" "Copper" 63.546
            -- Zn  Zinc        30  65,39 
            -- Ga  Gallium         31  69,723 
            -- Ge  Germanium       32  72,61 
            -- As  Arsenic         33  74,92159 
            -- Se  Selenium        34  78,96 
            -- Br  Bromine     35  79,904 
            -- Kr  Krypton         36  83,8 
            -- Rb  Rubidium        37  85,4678 
            -- Sr  Strontium       38  87,62 
            -- Y   Yttrium         39  88,90585 
            -- Zr  Zirconium       40  91,224 
            -- Nb  Niobium         41  92,90638 
            -- Mo  Molybdenum      42  95,94 
          , Element  43 "Tc" "Technetium" 98.9063 
            -- Ru  Ruthenium       44  101,07 
            -- Rh  Rhodium         45  102,9055 
            -- Pd  Palladium       46  106,42 
            -- Ag  Silver  47  107,8682 
            -- Cd  Cadmium         48  112,411 
            -- In  Indium      49  114,82 
            -- Sn  Tin     Latin Stannum   50  118,71 
            -- Sb  Antimony   51  121,75 
            -- Te  Tellurium       52  127,6 
            -- I   Iodine      53  126,90447 
            -- Xe  Xenon       54  131,29 
          , Element 55 "Cs" "Caesium" 132.90543 
          , Element 56 "Ba" "Barium" 137.327 
          , Element 57 "La" "Lanthanum" 138.9055 
          , Element 58 "Ce" "Cerium" 140.115 
          , Element 59 "Pr" "Praseodymium" 140.90765 
          , Element 60 "Nd" "Neodymium" 144.24 
          , Element 61 "Pm" "Promethium" 146.9151 
            -- Sm  Samarium        62  150,36 
            -- Eu  Europium        63  151,965 
            -- Gd  Gadolinium      64  157,25 
            -- Tb  Terbium         65  158,92534 
            -- Dy  Dysprosium      66  162,5 
            -- Ho  Holmium         67  164,93032 
            -- Er  Erbium      68  167,26 
            -- Tm  Thulium         69  168,93421 
            -- Yb  Ytterbium       70  173,04 
            -- Lu  Lutetium        71  174,967 
            -- Hf  Hafnium         72  178,49 
          , Element  73 "Ta" "Tantalum" 180.9479 
            -- W   Tungsten    German Wolfram  74  183,85 
            -- Re  Rhenium         75  186,207 
            -- Os  Osmium      76  190,2 
            -- Ir  Iridium         77  192,22 
            -- Pt  Platinum        78  195,08 
            -- Au  Gold    Latin Aurum     79  196,96654  
         , Element 80 "Hg" "Mercury" 200.59  
            -- Tl  Thallium        81  204,3833 
            -- Pb  Lead    Latin Plumbum   82  207,2 
            -- Bi  Bismuth         83  208,98037 
            -- Po  Polonium        84  208,9824 
            -- At  Astatine        85  209,9871 
          , Element 86 "Rn" "Radon" 222.0176 
          , Element 87 "Fr" "Francium" 223.0197 
          , Element 88 "Ra" "Radium" 226.0254 
          , Element 89 "Ac" "Actinium" 227.0278 
          , Element 90 "Th" "Thorium" 232.0381 
            -- Pa  Protactinium        91  231,0359 
            -- U   Uranium         92  238,0289 
            -- Np  Neptunium       93  237,0482 
            -- Pu  Plutonium       94  244,0642 
            -- Am  Americium       95  243,0614 
            -- Cm  Curium      96  247,0703 
          , Element 97 "Bk" "Berkelium" 247.0703 
            -- Cf  Californium         98  251,0796 
            -- Es  Einsteinium         99  252,0829 
            -- Fm  Fermium         100 257,0951 
            -- Md  Mendelevium         101 258,0986 
            -- No  Nobelium        102 259,1009 
            -- Lr  Lawrencium      103 260,1053 
          , Element  104 "Rf" "Rutherfordium" 261.1087 
            -- Db  Dubnium         105 262,1138 
            -- Sg  Seaborgium      106 263,1182 
            -- Bh  Bohrium         107 262,1229 
            -- Hs  Hassium         108 265 
            -- Mt  Meitnerium      109 266 
            -- Ds  Darmstadtium        110 269 
            -- Rg  Roentgenium         111 272 
          , Element 112 "Cn" "Copernicium" 285 
          , Element 113 "Uut" "Ununtrium" 284 
          , Element 114 "Uug" "Ununquadium" 289 
          , Element 115 "Uup" "Ununpentium" 288 
          , Element 116 "Uuh" "Ununhexium" 293 
          , Element 117 "Uus" "Ununseptium" 294     
          , Element 118 "Uuo" "Ununoctium" 294 ]
           

-- | Find element by its atomic number
--
-- > fmap atomicNumber (element 8) == Just 8 
element :: Int -> Maybe Element
element n = f n ptable
    where f :: Int -> [Element] -> Maybe Element
          f _ [] = Nothing
          f x (e:es) | atomicNumber e == x = Just e
                     | otherwise = f x es

-- | Find element by its symbol
--
-- > fmap atomicNumber (elementBySymbol "O") == Just 8 
elementBySymbol :: String -> Maybe Element
elementBySymbol ns = f ns ptable
    where f :: String -> [Element] -> Maybe Element
          f _ [] = Nothing
          f xs (e:es) | symbol e == xs = Just e
                     | otherwise = f xs es

-- | Show number of electrons in each shell
--
-- > let e = element 8
-- > shellElectrons e == [2, 6] 
shellElectrons :: Element -> [Int]
shellElectrons e = filter (> 0) $ f (fillShells (atomicNumber e)) 
        where f :: [(Int, Int, Int)] -> [Int]
              f ss = [sum (g n ss) | n <- [1..m]]
                where m = (length ss + 1) `div` 2
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
                                   
                 

              
              