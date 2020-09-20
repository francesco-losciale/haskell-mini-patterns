module NewTypeTask where

data Player = Player
    { playerHealth    :: Health
    , playerArmor     :: Armor
    , playerAttack    :: Attack
    , playerDexterity :: Dexterity
    , playerStrength  :: Strength
    }

newtype Health = Health {
    h :: Int
}    

newtype Armor = Armor {
    ar :: Int
}

newtype Attack = Attack {
    at :: Int
}

newtype Dexterity = Dexterity {
    dx :: Int
}

newtype Strength = Strength {
    s :: Int
}

newtype Damage = Damage {
    d :: Int
}


newtype Defense = Defense {
    ds :: Int
}

calculatePlayerDamage :: Attack -> Strength -> Damage
calculatePlayerDamage attack strength = Damage { d = (at attack) + (s strength) }

calculatePlayerDefense :: Armor -> Dexterity -> Defense
calculatePlayerDefense armor dexterity = Defense { ds = (ar armor) * (dx dexterity) }

calculateHit :: Damage -> Defense -> Health -> Health
calculateHit damage defense health = Health { h = (h health) + (ds defense) - (d damage) }

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