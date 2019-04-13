-- 20030019 scyll1 Luer Lyu
module Schelling
  ( Coord
  , AgentType (..)
  , Cell

  , step
  ) where

import Data.List
import System.Random

-- type-definition of a coordinate in our world
type Coord = (Int, Int)

-- data definition for the agent types
data AgentType 
  = Red       -- ^ Red agent
  | Green     -- ^ Green agent
  | Blue      -- ^ Blue agent
  deriving Eq -- Needed to compare for equality, otherwise would need to implement by ourself

-- Type-definition of a Cell: it is just a coordinate and an optional AgentType.
-- Note: the agent type is optional and is Nothing in case the cell is empty.
type Cell = (Coord, Maybe AgentType)

-- Computes one step of the Schelling Segregation model. The algorithm works as:
--  1. mark all unhappy agents
--  2. move all unhappy agents to any random Emtpy cell
--  3. all happy agents stay at the same position
step :: [Cell]           -- ^ All cells of the world
     -> StdGen           -- ^ The random-number generator
     -> Double           -- ^ The ratio of equal neighbours an agent requires to be happy
     -> ([Cell], StdGen) -- ^ Result is the new list of cells and the updated random-number generator
step cs g ratio = (cs', g')
    where
      es= filter isEmpty cs
      esl= length es --the length of the empty list
      not_es= cs\\es --the not empty list
      unhappy_cells=filter (\c->is_unhappy c ratio cs) not_es --take all the unhappy cells
      happy_cells= not_es\\unhappy_cells 
      (medium,g') = exchange es unhappy_cells g (esl-1) [] 
      cs'= medium++happy_cells --add the happy cells to the list

--find a random empty space to put the unhappy cell and reutrn a tuple
exchange :: [Cell]->[Cell]->StdGen->Int->[Cell]->([Cell],StdGen)
exchange es [] g _ cls= (es ++ cls,g)
exchange es (c:cs) g esl cls= exchange (c':(delete x es)) cs g' esl cls'
                           where 
                            (c',cls')= move c x cls
                            x= es !! k -- take one random element from empty list
                            (k,g')=randomR ((0,esl)::(Int,Int)) g --generate a random number

--exchange the content and add the unhappy to the happy list, and also return the new empty cell
move :: Cell->Cell->[Cell]->(Cell,[Cell])
move (a,b) (c,d) cls = ((a,d),((c,b):cls))

--tell which is the unhappy cell
is_unhappy :: Cell->Double->[Cell]->Bool
is_unhappy c ratio cs= if r_ratio<ratio then True
                       else False
                       where
                          r_ratio=fo (length same_cells) (length neighbors)
                          neighbors=filter (\x->isNeighbor x c) cs 
                          same_cells=filter (\x->isSame x c) neighbors 

--compare if two not empty cell have the same color                          
isSame :: Cell->Cell->Bool
isSame (_,Just Red) (_,Just Red)=True
isSame (_,Just Blue) (_,Just Blue)=True
isSame (_,Just Green) (_,Just Green)=True
isSame _ _=False

--take the the target cell as the fist parameter 
isNeighbor :: Cell->Cell->Bool 
isNeighbor ((a,b),_) ((c,d),_)= if (a+1==c &&b==d)  || (a-1==c && b==d)
                                || (a==c && b+1==d) || (a==c && b-1==d) 
                                || (a-1==c &&b-1==d)|| (a-1==c &&b+1==d)
                                || (a+1==c &&b-1==d)|| (a+1==c &&b+1==d)
                                then True
                                else False

--whether is an empty cell                                
isEmpty :: Cell->Bool
isEmpty (_, Nothing) = True
isEmpty _ = False

--get a double by dividing two int 
fo :: Int->Int->Double
fo a b= (fromIntegral a) / (fromIntegral b)