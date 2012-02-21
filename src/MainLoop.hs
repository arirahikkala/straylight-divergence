{-# LANGUAGE NoMonomorphismRestriction #-}
module MainLoop where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Random.Class
import Control.Monad.Identity
import Data.List
import Data.Ord
import Data.Accessor
import Game
import CommonTypes
import BasicTypes
import Object
import Action
import Interface
import AI (chooseAction)

mainLoop =
    do allObjects <- Map.assocs `fmap` gsGlobal references_
       aiActors <- filter ((/=playerCharRef) . ref) `fmap` (filterM takesActions $ map (uncurry SpecificObject) allObjects)
       gamestate <- gGlobal
       gameconfig <- askGlobal
       randomStates <- replicateM (length aiActors) getSplit
       let aiActions :: [(SpecificObject, (Initiative, Action))]
           aiActions = zipWith (\a r -> (runIdentity $ evalGameT (chooseAction a) r gamestate gameconfig)) aiActors randomStates
       -- AIs get to touch themselves but not the rest of the world when choosing actions
       -- (this allows them to save action plans made on their turn, etc.)
       mapM (\(o, _) -> touch (ref o) (obj o)) aiActions
       playerAction <- doInterface
       case playerAction of
         Quit -> return ()
         _ -> do
           playerObj <- dereferObj playerCharRef
           let po = SpecificObject playerCharRef playerObj
           mapM_ (\(a, (b, c)) -> resolveAction a b c) $ sortBy (comparing (fst . snd)) $ ((po, (0, playerAction)) : aiActions)
           mGlobal (currentTurn ^: succ)
           mainLoop

--chooseAction :: SpecificObject -> PureGame (SpecificObject, (Initiative, Action))
--chooseAction r = return (0, Idle)


{-
       actions <- sortBy (comparing fst) `fmap` map chooseAction actors
       mapM_ resolveAction $ sortBy (comparing fst) $ 
       mGlobal (currentTurn ^: succ)
-}

-- chooseAction returns (initiative, action)