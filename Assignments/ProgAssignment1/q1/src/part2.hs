import Lsystem
import System.Random

render' :: Int -> String -> System -> IO ()
render' n = renderSystem (mkStdGen n) (400,400)

main :: IO ()
main = do
       render' 42 "part2.svg" q12

q12 = System {                              
       systemBasis = [x] ,                 -- axiom: X
       systemRules = [ DeterministicRule {
                         ruleContext = ignoreContext ,
                         ruleCondition = unconditional ,
                         ruleMatch = matchDummy "X" ,
                         ruleReplacement = constantReplacement
                                            [ f,
                                              NodeBranch [[m,x]],x,
                                              NodeBranch [[p,x]],
                                              NodeBranch [[p,x]],f,m,
                                              NodeBranch [[m,x]],p,x,
                                              NodeBranch [[m,x]] ] } ,
                                            -- X -> F[-X]X[+X][+X]F-[-X]+X[-X]
                       DeterministicRule {
                         ruleContext = ignoreContext ,
                         ruleCondition = unconditional ,
                         ruleMatch = matchDummy "F" ,
                         ruleReplacement = constantReplacement [ f, f ] } ] ,
                                           -- F -> FF
       systemSteps = 4 } where
           x = NodeDummy [] "X"
           f = NodeDraw [] 1
           p = NodeRotate [] 4 0 0 -- roll number 2020114001
           m = NodeRotate [] (-4) 0 0
