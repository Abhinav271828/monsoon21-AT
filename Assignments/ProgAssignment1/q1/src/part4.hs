import Lsystem
import System.Random

render' :: Int -> String -> System -> IO ()
render' n = renderSystem (mkStdGen n) (400,400)

main :: IO ()
main = do
       render' 42 "part4.svg" q4

q14 = System {
       systemBasis = [f,p,f] ,             -- axiom: F+F
       systemRules = [ DeterministicRule {
                         ruleContext = ignoreContext ,
                         ruleCondition = unconditional ,
                         ruleMatch = matchF ,
                         ruleReplacement = constantReplacement [f,m,f,p,p,f,m,f] } ,
                                           -- F -> F-F++F-F
                       DeterministicRule {
                         ruleContext = contextMatch [] [f] [m,f] ,
                         ruleCondition = unconditional ,
                         ruleMatch = matchF ,
                         ruleReplacement = constantReplacement [f,m,p,f,p,f] } ] ,
                                           -- F⟨F⟩+F -> F-+F+F
       systemSteps = 9 } where
           f = NodeDraw [] 5
           p = NodeRotate [] (-90) 0 0
           m = NodeRotate [] 90 0 0
