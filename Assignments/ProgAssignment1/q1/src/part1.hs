import Lsystem
import System.Random

render' :: Int -> String -> System -> IO ()
render' n = renderSystem (mkStdGen n) (400,400)

main :: IO ()
main = do
       render' 42 "part1.svg" q11

q11 = System {
       systemBasis = [g] ,                 -- axiom G
       systemRules = [ DeterministicRule {
                         ruleContext = ignoreContext ,
                         ruleCondition = unconditional ,
                         ruleMatch = matchDummy "G" ,
                         ruleReplacement = constantReplacement [ x, m, g, m, x ] } ,
                                           -- G -> X-G-X
                       DeterministicRule {
                         ruleContext = ignoreContext ,
                         ruleCondition = unconditional ,
                         ruleMatch = matchDummy "X" ,
                         ruleReplacement = constantReplacement [ g, p, y, p, g ] } ,
                                           -- X -> G+Y+G
                       DeterministicRule {
                         ruleContext = ignoreContext ,
                         ruleCondition = unconditional ,
                         ruleMatch = matchDummy "Y" ,
                         ruleReplacement = constantReplacement
                                             [ NodeBranch [[p, f]], f, NodeBranch [[m, f]]] } ] ,
                                           -- Y -> [+F]F[-F]
       systemSteps = 9 } where
           g = NodeDummy [] "G"
           x = NodeDummy [] "X"
           y = NodeDummy [] "Y"
           f = NodeDraw [] 1
           p = NodeRotate [] (-30) 0 0
           m = NodeRotate [] 30 0 0
