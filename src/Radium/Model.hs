{- |
Module Radium.Model
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Molecule models.
-}

module Radium.Model ( ValenceModel(..) )where


-- | Bound type
data Bound = Single | Double | Triple

-- | This model describes molecules with valence bounds
data ValenceModel a = Atom a [(Bound, ValenceModel a)]
                    | Empty
