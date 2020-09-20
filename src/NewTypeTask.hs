module NewTypeTask where

-- Pattern
-- Newtype

-- Description
-- Lightweight data wrapper.

-- When to use
-- When using the same primitive type (Int, Text, etc.) to represent semantically different entities (name, title, description, etc.).

-- Benefits
-- Improves maintainability.
-- Increases code readability.
-- Enables writing custom instances.
-- Allows reusing instance definitions with DerivingVia.
-- Costs
-- Additional wrapping and unwrapping.
-- Deriving boilerplate is required to duplicate existing behaviour of the underlying type.

-- PS: think to primitive obsession when doing this test

data Player = Player
    { playerHealth    :: Health
    , playerArmor     :: Armor
    , playerAttack    :: Attack
    , playerDexterity :: Dexterity
    , playerStrength  :: Strength
    }
newtype Health = Health Int
newtype Armor = Armor Int
newtype Attack = Attack Int
newtype Dexterity = Dexterity Int
newtype Strength = Strength Int
newtype Damage = Damage Int
newtype Defense = Defense Int


calculatePlayerDamage :: Attack -> Strength -> Damage
calculatePlayerDamage (Attack attack) (Strength strength) = Damage (attack + strength) 

calculatePlayerDefense :: Armor -> Dexterity -> Defense
calculatePlayerDefense (Armor armor) (Dexterity dexterity) = Defense (armor * dexterity)

calculateHit :: Damage -> Defense -> Health -> Health
calculateHit (Damage damage) (Defense defense) (Health health) = Health (health + defense - damage)

-- The second player hits first player and the new first player is returned
hitPlayer :: Player -> Player -> Player
hitPlayer player1 player2 =
    let damage = calculatePlayerDamage
            (playerAttack player2)
            (playerStrength player2)
        defense = calculatePlayerDefense
            (playerArmor player1)
            (playerDexterity player1)
        newHealth = calculateHit
            damage
            defense
            (playerHealth player1)
    in player1 { playerHealth = newHealth }    