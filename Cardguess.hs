--  File     : Cardguess.hs
--  Author   : Wei Wang
--  Purpose  : Guess two cards

-- | This code partly implement the function of guessing cards,
--   that is, it can proceed normally, but seldom succeed to find the exact answer.
--	 The reason is that not all conditions are full considered, as a result, the list of
--   card is empty, however, it still does not guess the right answer, and finally throws 
--   an exception.
--   It still requires my effort to achieve a happy ending.


module Cardguess (initialGuess, nextGuess, GameState) where
import Card
import Data.List

-- |[Card] owns two cards on this condition
--  GameState store the remaining Cards
type GameState = [Card]

-- Feedback stores the status messages from the comparison between answer and guess 
type Feedback = (Int, Int, Int, Int, Int)

-- The initialGuessValues stores the cards for the first guess
initialGuessValues = [Card Club R5,Card Diamond R8]

-- A deck of cards, stores a deck of card
deck :: [Card]
deck = [Card s r | s <- [Club .. Spade], r <- [R2 .. Ace]]


-- | This is an initialGuess, and returns a initial guess
--   and the GameState store a deck of card, where the initial guess cards are not included.
initialGuess :: Int -> ([Card], GameState)
initialGuess cardNumber
	| cardNumber == 2 = (initialGuessValues,
					deleteCards initialGuessValues deck)
	| otherwise = error "This program only guesses two cards!"

-- | This is nextGuss code, for proceeding next guess, which returns
--   the next guess cards and the next game state,
--   by way of invoking another function
nextGuess :: ([Card], GameState) -> Feedback ->([Card], GameState)
nextGuess (presentGuess,presentState) feedback =
	csProcessing (presentGuess,presentState) feedback
	
-- | process the number of correct suits from feedback
--   and return the next card guess and the game state.
csProcessing :: ([Card],GameState) -> Feedback -> ([Card],GameState)
csProcessing ([Card s1 r1,Card s2 r2],remainingCards) (cC,lR,cR,hR,cS) =
-- if the correct suit is zero, and then remove the cards which own the suit
	if cS == 0 then 
		(crProcessing 
			([Card s1 r1,Card s2 r2],(deleteCardsBySuit (Card s1 r1) (deleteCardsBySuit (Card s2 r2) remainingCards))) lR cR hR)
-- if the correct suit is one, choose two cards with same suit the first suit in the previous guess 									
	else if cS == 1 then
		let oneCard = chooseCardBySuit s1 remainingCards
		in
			let twoCards = [oneCard,chooseCardBySuit s1 (deleteCards [oneCard] remainingCards)]
			in(twoCards,(Card s2 r2) : (deleteCards twoCards remainingCards))
	else
		if cR /=2 then crProcessing ([Card s1 r1,Card s2 r2],remainingCards) lR cR hR	
-- When the number of both suit and rank are two, exchange the suits of two cards. 
		else ([Card s2 r1,Card s1 r2],remainingCards)


-- | handle the condition of correct rank							
crProcessing :: ([Card],GameState) -> Int -> Int -> Int -> ([Card],GameState)
crProcessing ([Card s1 r1,Card s2 r2],cards) lR cR hR = 

	if cR == 0 then 
-- | when the lowerRanks is two, choose two cards whose ranks are below the lower rank of previous guess,
--   and the game state is the cards whose ranks are all below the lower rank of the previous guess.  
		if lR == 2 then 
			(getTwoCards [Card s1 r1,Card s2 r2] cards deleteCardsByGTERank,
				(deleteCards (getTwoCards [Card s1 r1,Card s2 r2] cards deleteCardsByGTERank)
					(deleteCardsByGTERank [Card s1 r1,Card s2 r2] cards)))
-- | when the higherRanks is two, choose two cards whose ranks are above the higher rank of previous guess,
--   and the game state is the cards whose ranks are all below the higher rank of the previous guess. 
		else if hR == 2 then 
			(getTwoCards [Card s1 r1,Card s2 r2] cards deleteCardsByLTERank ,
				(deleteCards (getTwoCards [Card s1 r1,Card s2 r2] cards deleteCardsByLTERank)
					(deleteCardsByLTERank [Card s1 r1,Card s2 r2] cards)))
-- | when both higherRanks and lowerRanks are one, choose one card whose rank is above the higher rank of previous guess,
--   and another card whose rank is below the lower rank of previous guess.
--   The game state is the cards whose ranks are outside of the ranks in previous guess. 					
		else if hR == 1 && lR == 1 then 
			(getTwoCards [Card s1 r1,Card s2 r2] cards deleteCardsBetweenRank ,
				(deleteCards (getTwoCards [Card s1 r1,Card s2 r2] cards deleteCardsBetweenRank) 
					(deleteCardsBetweenRank [Card s1 r1,Card s2 r2] cards)))
-- | when higherRanks is one and lowRanks is zero, choose one card between the ranks of previous guess,
--   and another card whose rank is above the higher rank of previous guess.
--   The game state is the cards whose ranks are between and above the ranks of the previous guess. 						
		else if hR == 1 && lR == 0 then 
			([chooseOneCardByFunc head (deleteCardsBeyondRank [Card s1 r1,Card s2 r2] cards ),
				chooseOneCardByFunc head (rightRankCards [Card s1 r1,Card s2 r2] cards )],
					((deleteCards 
						[chooseOneCardByFunc head (deleteCardsBeyondRank [Card s1 r1,Card s2 r2] cards ),
							chooseOneCardByFunc head (rightRankCards [Card s1 r1,Card s2 r2] cards )]
								((deleteCardsBeyondRank [Card s1 r1,Card s2 r2] cards) ++ (rightRankCards [Card s1 r1,Card s2 r2] cards)))))
-- | when higherRanks is zero and lowRanks is one, choose one card between the ranks of previous guess,
--   and another card whose rank is below the lower rank of previous guess.
--   The game state is the cards whose ranks are between and below the ranks of the previous guess. 
		else if hR == 0 && lR == 1 then 
			([chooseOneCardByFunc head (leftRankCards [Card s1 r1,Card s2 r2] cards ),
				chooseOneCardByFunc head (deleteCardsBeyondRank [Card s1 r1,Card s2 r2] cards )],
					((deleteCards 
						[chooseOneCardByFunc head (leftRankCards [Card s1 r1,Card s2 r2] cards ),
							chooseOneCardByFunc head (deleteCardsBeyondRank [Card s1 r1,Card s2 r2] cards )]
								((leftRankCards [Card s1 r1,Card s2 r2] cards) ++ (deleteCardsBeyondRank [Card s1 r1,Card s2 r2] cards)))))	
-- | when both ranks of higherRanks and lowerRanks are zero, choose two cards between the ranks of previous guess.
--   The game state is the cards whose ranks are between the ranks of the previous guess. 								
		else (getTwoCards [Card s1 r1,Card s2 r2] cards deleteCardsBeyondRank,
				(deleteCards (getTwoCards [Card s1 r1,Card s2 r2] cards deleteCardsBeyondRank)
					(deleteCardsBeyondRank [Card s1 r1,Card s2 r2] cards)))	
	else if cR == 1
--   When the correctRanks is one, I cannot decide which one is correct, and that's the point that need to be improved.
		then if lR == 1 && hR == 0 || lR == 0 && hR == 1 then 
				let Card chooseS chooseR = chooseOneCardByFunc head (leftRankCards [Card s1 r1,Card s2 r2] cards )
				in([Card chooseS chooseR,Card s1 r1],(Card s2 r2) : (deleteCards [Card chooseS chooseR] cards))									
		else let Card chooseS chooseR = chooseOneCardByFunc head (deleteCardsBeyondRank [Card s1 r1,Card s2 r2] cards )
			 in([Card chooseS chooseR,Card s2 r2],(Card s1 r1) : (deleteCards [Card chooseS chooseR] cards))
--   When the correctRanks is two, just exchange the suits.
	else ([Card s2 r1,Card s1 r2],cards)

--   get two cards from given card lists by function							
getTwoCards :: [Card] -> [Card] -> ([Card] -> [Card] -> [Card]) -> [Card]
getTwoCards [] [] _ = []
getTwoCards	[Card s1 r1,Card s2 r2] cards func = 
	let Card sh rh = head $ func [Card s1 r1,Card s2 r2] cards
	    Card sl rl = last $ func [Card s1 r1,Card s2 r2] cards
	in
	if rh <= rl then [Card sh rh,Card sl rl]
	else [Card sl rl,Card sh rh]

	
--   choose one card by a different suit	
chooseCardBySuit :: Suit -> [Card] -> Card
chooseCardBySuit _ [] = error "No Card"
chooseCardBySuit suit (Card s r:xs) 
	| suit == s = Card s r
	| otherwise = chooseCardBySuit suit xs	

--   choose one card from a card list by function							
chooseOneCardByFunc :: ([Card] -> Card) -> [Card] -> Card
chooseOneCardByFunc _ [] = error "No Card"
chooseOneCardByFunc func cardList = 
	func cardList

--   delete given cards from a list of cards 
deleteCards :: [Card] -> [Card] -> [Card]
deleteCards [] [] = []
deleteCards [] source = source
deleteCards (x:xs) source
	| null (x:xs) == False = deleteCards xs $ delete x source
	| otherwise = source
	
--   delete cards by suit
deleteCardsBySuit :: Card -> [Card] -> [Card]
deleteCardsBySuit _ [] = []
deleteCardsBySuit (Card s r) (Card s0 r0:xs) = 
	if null (Card s0 r0:xs) == False then 
		if s0 /= s then (Card s0 r0) : deleteCardsBySuit (Card s r) xs
		else deleteCardsBySuit (Card s r) $ deleteCards [Card s0 r0] (Card s0 r0:xs)
	else
		(Card s0 r0:xs)

-- | delete cards which own less or equal rank which is a higher rank in the guess
--   for the condition of higherRanks == 2
deleteCardsByLTERank :: [Card] -> [Card] -> [Card]
deleteCardsByLTERank _ [] = []
deleteCardsByLTERank [Card s1 r1,Card s2 r2] (Card s0 r0:xs) = 
	if null (Card s0 r0:xs) == False then
		if r1 >= r2 then
			if r0 > r1 then (Card s0 r0) : deleteCardsByLTERank [Card s1 r1,Card s2 r2] xs
			else deleteCardsByLTERank [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs)
		else if r0 > r2 then (Card s0 r0) : deleteCardsByLTERank [Card s1 r1,Card s2 r2] xs
			else deleteCardsByLTERank [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs)
	else
		(Card s0 r0:xs)

-- | delete cards which own greater or equal rank which is a lower rank in the guess
--   for the condition of lowerRanks == 2		
deleteCardsByGTERank :: [Card] -> [Card] -> [Card]
deleteCardsByGTERank _ [] = []
deleteCardsByGTERank [Card s1 r1,Card s2 r2] (Card s0 r0:xs) = 
	if null (Card s0 r0:xs) == False then
		if r1 <= r2 then
			if r0 < r1 then (Card s0 r0) : deleteCardsByGTERank [Card s1 r1,Card s2 r2] xs
			else deleteCardsByGTERank [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs)
		else if r0 < r2 then (Card s0 r0) : deleteCardsByGTERank [Card s1 r1,Card s2 r2] xs
			else deleteCardsByGTERank [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs) 
	else
		(Card s0 r0:xs)		

-- | delete cards beyond ranks, namely, retain the cards 
--   between rank1 and rank2 (not included), if rank1<=rank2 
deleteCardsBeyondRank :: [Card] -> [Card] -> [Card]
deleteCardsBeyondRank _ [] = []
deleteCardsBeyondRank [Card s1 r1,Card s2 r2] (Card s0 r0:xs) =	
	if null (Card s0 r0:xs) == False then
		if r1 <= r2 then
			if r0 < r2 && r0 > r1 then (Card s0 r0) : deleteCardsBeyondRank [Card s1 r1,Card s2 r2] xs
			else deleteCardsBeyondRank [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs)
		else if r0 < r1 && r0 > r2 then (Card s0 r0) : deleteCardsBeyondRank [Card s1 r1,Card s2 r2] xs
			else deleteCardsBeyondRank [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs)
	else
		(Card s0 r0:xs)	

-- | get two list of cards, the left card list store the cards whose ranks are lower than the smaller rank in r1 and r2,
--	 and another card list store the cards whose ranks are higher than the larger rank	in r1 and r2
--   deleteCardsBetweenRank :: [Card] -> [Card] -> ([Card],[Card])
deleteCardsBetweenRank :: [Card] -> [Card] -> [Card]
deleteCardsBetweenRank _ [] = []
deleteCardsBetweenRank [Card s1 r1,Card s2 r2] (Card s0 r0:xs) =
	leftRankCards [Card s1 r1,Card s2 r2] (Card s0 r0:xs) ++ 
		rightRankCards [Card s1 r1,Card s2 r2] (Card s0 r0:xs)

--   get the card list whose ranks are below the smaller rank in r1 and r2
leftRankCards :: [Card] -> [Card] -> [Card]
leftRankCards _ [] = []
leftRankCards [Card s1 r1,Card s2 r2] (Card s0 r0:xs) =
	if null (Card s0 r0:xs) == False then
		if r1 <= r2 then
			if r0 < r1 then (Card s0 r0) : leftRankCards [Card s1 r1,Card s2 r2] xs
			else leftRankCards [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs)
		else if r0 < r2 then (Card s0 r0) : leftRankCards [Card s1 r1,Card s2 r2] xs
			else leftRankCards [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs)
	else		
		(Card s0 r0:xs)	
		
--   get the card list whose ranks are above the smaller rank in r1 and r2		
rightRankCards :: [Card] -> [Card] -> [Card]
rightRankCards _ [] = []
rightRankCards [Card s1 r1,Card s2 r2] (Card s0 r0:xs) =
	if null (Card s0 r0:xs) == False then
		if r1 <= r2 then
			if r0 > r2 then (Card s0 r0) : rightRankCards [Card s1 r1,Card s2 r2] xs
			else rightRankCards [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs)
		else if r0 > r1 then (Card s0 r0) : rightRankCards [Card s1 r1,Card s2 r2] xs
			else rightRankCards [Card s1 r1,Card s2 r2] $ deleteCards [Card s0 r0] (Card s0 r0:xs)
	else		
		(Card s0 r0:xs)	