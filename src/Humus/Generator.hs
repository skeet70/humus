module Humus.Generator (
  getLands,
  numLands,
) where


import Humus.Card

-- Twin Exarch (Combo): 24/60, cmc 2.3
-- Bloom Titan (Combo): 27/60, cmc 3.0
-- Boss Sligh (Aggro): 17/60, cmc 1.35
-- Abzan (Midrange): 24/60, cmc 3.47
-- U/B Control (Control): 25/60, cmc 3.86
-- Sidisi Whip (Midrange): 23/60, cmc 3.59
-- Expectation:
--    (m*n)/60
-- is the number of lands in a average 7 card hand. For Modern this looks to be
-- about .5 over the average CMC for the deck, and for Standard it looks like
-- it's about 1 under.
numLands :: Deck -> Curve -> Int
numLands = undefined
--numLands deck curve = undefined


getLands :: String -> Deck -> Curve -> [(Color, Float)] -> Deck
getLands = undefined
--getLands format deck curve distribution = undefined

-- Using that information we calculate how many lands they need. This is based
-- off the mana curve and how color intensive their deck is. A low mana curve
-- moves our number of lands down, more colors will move it back up from there.

-- Combine the knowledge of how many total lands we need in the deck with the
-- knowledge of what the distribution of colors and mana costs is to get the
-- lands we need. We'll be getting them out of our known lands that will be
-- legal in this format... not sure about that part yet.
