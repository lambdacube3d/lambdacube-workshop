{-# LANGUAGE RecordWildCards #-}

module Logic
  ( Spaceship(..)
  , Asteroid(..)
  , Bullet(..)
  , Particle(..)
  , World(..)
  , UserInput(..)
  , stepGame
  , world0
  ) where

import Control.Monad.Random.Class (MonadRandom(..))
import Data.Fixed (mod')
import Data.Maybe
import Data.List (foldl', partition)
import Data.Vect
import Data.Vect.Float.Instances
import Data.Vect.Float.Util.Dim2 (sinCos)

data Spaceship
  = Spaceship
  { sPosition :: Vec2
  , sVelocity :: Vec2
  , sRadius   :: Float
  , sAngle    :: Float
  , sGunRechargeTime :: Float
  }

data Asteroid
  = Asteroid
  { aPosition :: Vec2
  , aVelocity :: Vec2
  , aRadius   :: Float
  }

data Bullet
  = Bullet
  { bPosition :: Vec2
  , bVelocity :: Vec2
  , bRadius   :: Float
  , bLifetime :: Float
  , bAngle    :: Float
  } deriving Show

data Particle
  = Particle
  { pPosition     :: Vec2
  , pVelocity     :: Vec2
  , pRadius       :: Float
--  , pAngle        :: Float
--  , pRadialSpeed  :: Vec2
  , pLifetime     :: Float
  } deriving Show

data World
  = World
  { spaceship :: Maybe Spaceship
  , asteroids :: [Asteroid]
  , bullets   :: [Bullet]
  , particles :: [Particle]
  }

data UserInput
  = UserInput
  { leftPressed     :: Bool
  , rightPressed    :: Bool
  , forwardPressed  :: Bool
  , backwardPressed :: Bool
  , spacePressed    :: Bool
  , enterPressed    :: Bool
  }

spaceship0 = Spaceship zero zero 0.5 0 0
world0 = World
  { spaceship = Just spaceship0
  , asteroids = [Asteroid (Vec2 2 2) (Vec2 0.1 0.2) 1.5, Asteroid (Vec2 0 4) (Vec2 0.01 0.04) 0.5]
  , bullets   = []
  , particles = []
  }

perspectiveScale = 5 :: Float
screenHalfWidth = 1 * perspectiveScale :: Float
screenHalfHeight = 1 * perspectiveScale ::  Float
minAsteroidRadius = 0.1 :: Float

wrapToScreen :: Vec2 -> Vec2
wrapToScreen (Vec2 x y) = Vec2 (mod' (x + screenHalfWidth) (2*screenHalfWidth) - screenHalfWidth) (mod' (y + screenHalfHeight) (2*screenHalfHeight) - screenHalfHeight)

stepSpaceship :: Float -> UserInput -> Spaceship -> ([Particle], [Bullet], Spaceship)
stepSpaceship deltaTime UserInput{..} a@Spaceship{..} = (newParticles, newBullets, nextSpaceship) where
  nextSpaceship = a
    { sPosition = wrapToScreen $ sPosition + sVelocity &* deltaTime
    , sAngle    = sAngle + deltaTime * case (leftPressed, rightPressed) of
        (True,False) -> 5
        (False,True) -> -5
        _ -> 0
    , sVelocity = sVelocity + if forwardPressed then direction &* deltaTime else zero
    , sGunRechargeTime = max (if null newBullets then 0 else 0.4) (sGunRechargeTime - deltaTime)
    }
  direction = sinCos sAngle
  newParticles = if forwardPressed then [Particle sPosition (-direction &* 8) 0.3 0.2] else []
  newBullets = case spacePressed && sGunRechargeTime <= 0 of
    False -> []
    True  -> [Bullet sPosition (sVelocity + sinCos sAngle &* 1) 0.15 3 sAngle]

stepAsteroid :: Float -> Asteroid -> Asteroid
stepAsteroid deltaTime a@Asteroid{..} = a {aPosition = wrapToScreen $ aPosition + aVelocity &* deltaTime}

stepBullet :: Float -> Bullet -> Maybe Bullet
stepBullet deltaTime a@Bullet{..}
  | bLifetime > 0 = Just $ a {bPosition = wrapToScreen $ bPosition + bVelocity &* deltaTime, bLifetime = bLifetime - deltaTime}
  | otherwise = Nothing

stepParticle :: Float -> Particle -> Maybe Particle
stepParticle deltaTime a@Particle{..}
  | pLifetime > 0 = Just $ a {pPosition = pPosition + pVelocity &* deltaTime, pLifetime = pLifetime - deltaTime}
  | otherwise = Nothing

stepWorld :: Float -> UserInput -> World -> World
stepWorld deltaTime userInput World{..}
  | (null asteroids || isNothing spaceship) && enterPressed userInput = world0
  | otherwise = World
    { spaceship = nextSpaceship
    , asteroids = map (stepAsteroid deltaTime) asteroids
    , bullets   = [a | Just a <- map (stepBullet deltaTime) $ newBullets ++ bullets]
    , particles = [a | Just a <- map (stepParticle deltaTime) $ newParticles ++ particles]
    }
    where
      (newParticles, newBullets, nextSpaceship) = case spaceship of
        Just spaceship -> let (a,b,c) = stepSpaceship deltaTime userInput spaceship in (a,b,Just c)
        _ -> ([],[],Nothing)

collideSpaceshipAsteroid :: Spaceship -> Asteroid -> Bool
collideSpaceshipAsteroid Spaceship{..} Asteroid{..} = len (sPosition - aPosition) < sRadius + aRadius

collideBulletAsteroid :: Bullet -> Asteroid -> Bool
collideBulletAsteroid Bullet{..} Asteroid{..} = len (bPosition - aPosition) < bRadius + aRadius

collideAsteroidsBullet :: [Asteroid] -> Bullet -> Bool
collideAsteroidsBullet asteroids bullet = any (collideBulletAsteroid bullet) asteroids

collideBulletsAsteroid :: MonadRandom m => [Bullet] -> Asteroid -> m ([Asteroid], [Particle])
collideBulletsAsteroid bullets asteroid
      | any (flip collideBulletAsteroid asteroid) bullets = breakAsteroid asteroid
      | otherwise = return ([asteroid],[])

breakAsteroid :: MonadRandom m => Asteroid -> m ([Asteroid], [Particle])
breakAsteroid Asteroid{..} = do
  let n = 3
      aRadius' = aRadius / 2
  pieces <- sequence
    [ do rv <- getRandomR (0.1, 1.0 :: Float)
         ra <- (/ n) <$> getRandomR (0.0, 1.0)
         return $ Asteroid aPosition (sinCos (((i / n) + ra) * 2 * pi) &* len aVelocity) aRadius'
    | i <- [0..n], aRadius' >= minAsteroidRadius
    ]
  let n = 1
  explosion <- sequence
    [ do rv <- getRandomR (0.1, 1.0 :: Float)
         ra <- (/ n) <$> getRandomR (0.0, 1.0)
         return $ Particle aPosition (sinCos (((i / n) + ra) * 2 * pi) &* len aVelocity &* rv) 1 1
    | i <- [0..n]
    ]
  return (pieces, explosion)

collideWorld :: MonadRandom m => World -> m World
collideWorld World{..} = do
  (ateroids', asteroidExplosion) <- mconcat <$> mapM (collideBulletsAsteroid bullets) asteroids
  spaceshipExplosion <- case (spaceship,spaceship') of
      (Just Spaceship{..}, Nothing) -> do
        let n = 6
        sequence
          [ do rv <- getRandomR (0.1, 1.0 :: Float)
               ra <- (/ n) <$> getRandomR (0.0, 1.0)
               return $ Particle sPosition (sinCos (((i / n) + ra) * 2 * pi) &* len sVelocity &* rv) 1 2
          | i <- [0..n]
          ]
      _ -> return []
  return $ World spaceship' ateroids' bullets' (spaceshipExplosion ++ asteroidExplosion ++ particles)
  where
    spaceship' = case spaceship of
      Just spaceship -> if any (collideSpaceshipAsteroid spaceship) asteroids then Nothing else Just spaceship
      Nothing -> Nothing
    bullets' = filter (not . collideAsteroidsBullet asteroids) bullets

stepGame :: MonadRandom m => Float -> UserInput -> World -> m World
stepGame deltaTime userInput = {-collideWorld-} pure . stepWorld deltaTime userInput
