{- a simple descriptive language for the scenes to be rendered. -}
module Lang (runSceneParser, evaluate)
    where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data
import Vector

----------- Structures -------------
data Struct = SSurface String [Element]
            | SSphere String [Element]
            | SLight String [Element]
            | SBGColor Color
            | SSize (Int,Int)
              deriving Show

data Element = EFloat String Double
             | EString String String
             | EVector String Vector
               deriving Show
getTerm :: Element -> String
getTerm (EFloat s _) = s
getTerm (EString s _) = s
getTerm (EVector s _) = s

getFloatValue (EFloat _ v) = v
getStringValue (EString _ v) = v
getVectorValue (EVector _ v) = v
----------- THE PARSER -------------
spaces :: Parser ()
spaces = skipMany1 space
spaceOrNo = skipMany space

parseFloat :: Parser Double
parseFloat =(try parseNDotFloat) <|> (try parseDotFloat) <|> parseIntFloat

parseIntFloat :: Parser Double
parseIntFloat = (char '-' >> (liftM (negate . read) $ many1 digit)) 
                <|> (liftM read $ many1 digit)
parseDotFloat = do
  a <- many1 digit
  char '.'
  b <- many1 digit
  return $ read $ a ++ "." ++ b
parseNDotFloat = do
  char '-'
  a <- parseDotFloat
  return $ negate a

parseEFloat :: Parser Element
parseEFloat = do
  name <- parseElementHead
  a <- parseFloat
  return $ EFloat name a

parseString :: Parser String
parseString = many1 strChar

strChar = noneOf " \t\n\"':/\\{}"

parseEString :: Parser Element
parseEString = do
  name <- parseElementHead
  val <- parseString
  return $ EString name val

parseVector :: Parser Vector
parseVector = do
  vs <- between (char '(' >> spaceOrNo) (spaceOrNo >> char ')')
        (parseFloat `sepBy` try (spaceOrNo >> char ',' >> spaceOrNo))
  return (vs!!0, vs!!1, vs!!2)

parseEVector :: Parser Element
parseEVector = do
  name <- parseElementHead
  val <- parseVector
  return $ EVector name val

parseElementHead :: Parser String
parseElementHead = do
  name <- parseString
  spaceOrNo
  char '='
  spaceOrNo
  return name

parseStructHead :: String -> Parser String
parseStructHead s = do
  string s
  spaces
  name <- parseString
  spaces
  return name

braces = between (char '{' >> skipMany space) (skipMany space >> char '}')

parseSurface :: Parser Struct
parseSurface = do
  name <- parseStructHead "Surface"
  elems <- braces ((choice [try parseEFloat, try parseEVector]) `sepEndBy` spaces)
  return $ SSurface name elems

parseSphere :: Parser Struct
parseSphere = do
  name <- parseStructHead "Sphere"
  elems <- braces ((choice [try parseEFloat, try parseEVector, try parseEString]) `sepEndBy` spaces)
  return $ SSphere name elems

parseLight :: Parser Struct
parseLight = do
  name <- parseStructHead "Light"
  elems <- braces ((choice [try parseEVector, try parseEFloat]) `sepEndBy` spaces)
  return $ SLight name elems

parseBGColor :: Parser Struct
parseBGColor = do
  string "Background"
  spaces
  color <- parseVector
  return $ SBGColor color

parseSize :: Parser Struct
parseSize = do
  string "Size"
  spaces
  vs <- between (char '(' >> spaceOrNo) (spaceOrNo >> char ')') 
        (parseFloat `sepBy` try (spaceOrNo >> char ',' >> spaceOrNo))
  return $ SSize (floor $ vs!!0, floor $ vs!!1)

sceneParser :: Parser [Struct]
sceneParser = (choice [try parseSurface, try parseSphere, try parseLight,try parseBGColor, try parseSize]) 
              `sepEndBy` spaces

runSceneParser input = parse sceneParser "SceneParser" input
-------------- Evaluation ------------------
makeSurface :: Struct -> (String, Surface)
makeSurface (SSurface name elems) = (name, surf)
    where surf = foldl mergeNew defaultSurface elems
          mergeNew oldSurf elem = changeTerm (getTerm elem) elem oldSurf
          changeTerm "Ambient" elem oldSurf = oldSurf { sAmbient = getFloatValue elem }
          changeTerm "Diffuse" elem oldSurf = oldSurf { sDiffuse = getFloatValue elem }
          changeTerm "Specular" elem oldSurf = oldSurf { sSpecular = getFloatValue elem }
          changeTerm "Shininess" elem oldSurf = oldSurf { sShininess = floor $ getFloatValue elem }
          changeTerm "Reflection" elem oldSurf = oldSurf { sReflection = getFloatValue elem }
          changeTerm "Transmit" elem oldSurf = oldSurf { sTransmit = getFloatValue elem }
          changeTerm "Color" elem oldSurf = oldSurf { sColor = getVectorValue elem }
          changeTerm _ _ oldSurf = oldSurf

makeSphere :: [(String,Surface)] -> Struct  -> Object
makeSphere surfList (SSphere name elems) = sphere
    where emptySphere = Sphere 0 (0,0,0) defaultSurface
          sphere = foldl mergeNew emptySphere elems
          mergeNew oldSph elem = changeTerm (getTerm elem) elem oldSph
          changeTerm "Radius" elem oldSph = oldSph { radius = getFloatValue elem }
          changeTerm "Center" elem oldSph = oldSph { center = getVectorValue elem }
          changeTerm "Surface" elem oldSph = oldSph { surface = fromJust (lookup (getStringValue elem) surfList) }
          changeTerm _ _ oldSph = oldSph

makeLight :: Struct -> Light
makeLight (SLight name elems) = light
    where noLight = Light (0,0,0) 0
          light = foldl mergeNew noLight elems
          mergeNew oldLight elem = changeTerm (getTerm elem) elem oldLight
          changeTerm "Position" elem oldLight = oldLight { position = getVectorValue elem }
          changeTerm "Intensity" elem oldLight = oldLight { intensity = getFloatValue elem }
          changeTerm _ _ oldLight = oldLight

evaluate :: [Struct] -> Scene
evaluate sl = Scene { objects = objs,
                      lights = lghts,
                      backgroundColor = color,
                      width = w,
                      height = h
                    }
    where surfList = ("defaultSurface",defaultSurface):(map makeSurface $ filter isSurface sl)
          objs = map (makeSphere surfList) $ filter isSphere sl
          lghts = map makeLight $ filter isLight sl
          color = case length (filter isBGColor sl) of
                    0 -> defaultBackgroundColor
                    _ -> getColor $ head $ filter isBGColor sl
          getColor (SBGColor c) = c
          (w,h) = case length (filter isSize sl) of
                    0 -> (imageWidth, imageHeight)
                    _ -> getWH $ head $ filter isSize sl
          getWH (SSize s) = s
          isSurface (SSurface _ _) = True
          isSurface _ = False
          isSphere (SSphere _ _) = True
          isSphere _ = False
          isLight (SLight _ _) = True
          isLight _ = False
          isBGColor (SBGColor _) = True
          isBGColor _ = False
          isSize (SSize _) = True
          isSize _ = False
