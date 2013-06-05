module CardGame where

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Eq, Show)
    
-- 2 .. 9, j, q, k, * (Ace)
type Value = Char

type Card = (Suit, Value)
    
type Deck = [ Card ]

data Player = North | South | East | West
    deriving (Eq, Show)

-- 6.56
-- The head leads
type Trick = [ (Player, Card) ]

sampleTrick :: Trick
sampleTrick = [ (North, (Hearts, 'k')), (South,(Hearts,'2')), (East,(Hearts,'j')), (West,(Hearts,'1')) ]

sampleTrickWithTrump :: Trick
sampleTrickWithTrump = [ (North, (Diamonds, '3')), (South,(Clubs,'*')), (East,(Diamonds,'7')), (West,(Diamonds,'9')) ]

-- 6.57
-- Assuming no Trumps
    
winNT :: Trick -> Player
winNT trick = head [ p | (p,v) <- player_values, v == max_value ]
    where
        max_value = maximum [ v | (p,(s,v)) <- trick ]
        player_values = [ (p, v) | (p,(s,v)) <- trick ]
        
-- 6.58
winT :: Suit -> Trick -> Player
winT trump trick = winNT trumps
    where
        trumps = [ (p,(s,v)) | (p,(s,v)) <- trick, s == trump ]
        
-- 6.59
type Hand = (Player,[Card])

-- 6.60
type Hands = [Hand]

sampleHands :: Hands
sampleHands = [ (North,[(Spades,'*'),(Spades,'2'),(Hearts,'k'),(Hearts,'7'),(Diamonds,'3')]), 
                          (South,[(Spades,'k'),(Spades,'q'),(Spades,'j'),(Hearts,'2'),(Clubs,'*')]), 
                          (East,[(Spades,'3'),(Hearts,'j'),(Diamonds,'7'),(Diamonds,'2'),(Clubs,'9')]), 
                          (West,[(Spades,'6'),(Diamonds,'9'),(Diamonds,'5'),(Clubs,'k'),(Clubs,'8')]) ]

-- 6.61
-- the play in a trick must be possible and legal
-- possible :: the cards played in each player should be in their hand
-- legal :: players should follow the suit of the leader if they can

samplePossibleTrick :: Trick
samplePossibleTrick = [ (North, (Hearts, '7')), (South,(Hearts,'2')), (East,(Hearts,'j')), (West,(Clubs,'8')) ]

sampleImpossibleTrick :: Trick
sampleImpossibleTrick = [ (North, (Hearts, 'j')), (South,(Hearts,'2')), (East,(Hearts,'j')), (West,(Hearts,'1')) ]

playerHasCard :: Player -> Card -> Hands -> Bool
playerHasCard p c hs = elem c player_cards
    where
        player_cards = head [ cards | (player,cards) <- hs, player == p ]

trickIsPossible :: Hands -> Trick -> Bool
trickIsPossible hs trick = and [ playerHasCard p c hs  | (p,c) <- trick ]

cantFollowSuit :: Player -> Suit -> Hands -> Bool
cantFollowSuit p s hs = null [ suit | (suit,value) <- hand, s == suit ]
    where
        (player, hand) = head [ (player,hand) | (player,hand) <- hs, p == player]

trickIsLegal :: Hands -> Trick -> Bool
trickIsLegal hs trick = and [ s == lead_suit || cantFollowSuit p lead_suit hs | (p,(s,v)) <- trick ]
    where
        (player,(lead_suit,_)) = head trick

checkPlay :: Hands -> Trick -> Bool
checkPlay hs t = trickIsPossible hs t && trickIsLegal hs t  