import Lsystem
import System.Random

render' :: Int -> String -> System -> IO ()
render' n = renderSystem (mkStdGen n) (400,400)

main :: IO ()
main = do
       render' 42 "part3_1.svg" q13
       render' 37 "part3_2.svg" q13
       render' 57 "part3_3.svg" q13
       render' 64 "part3_4.svg" q13
       render' 99 "part3_5.svg" q13

q13 = System {
       systemBasis = [x] ,                                       -- axiom: X
       systemRules = [ StochasticRule [(0.5, DeterministicRule {
                                               ruleContext = ignoreContext ,
                                               ruleCondition = unconditional ,
                                               ruleMatch = matchDummy "X" ,
                                               ruleReplacement = constantReplacement
                                                    [f , m ,
                                                     NodeBranch [[ NodeBranch [[m,x]] ,
                                                                   p,x ]], p, f,
                                                     NodeBranch [[p,f,x]], m, x ] } ),
                                                                 -- 50% chance X -> F-[[-X]+X]+F[+FX]-X
                                       (0.5, DeterministicRule {
                                               ruleContext = ignoreContext ,
                                               ruleCondition = unconditional ,
                                               ruleMatch = matchDummy "X" ,
                                               ruleReplacement = constantReplacement
                                                   [f , p ,
                                                    NodeBranch [[ NodeBranch [[p,x]] ,
                                                                  m , x]] , m , f ,
                                                    NodeBranch [[ m , f , x ]], p, x,
                                                    NodeBranch [[ x ]] ] } ) ],
                                                                 -- 50% chance X -> F+[[+X]-X]-F[-FX]+X[X]
                       StochasticRule [(0.33, DeterministicRule {
                                                ruleContext = ignoreContext ,
                                                ruleCondition = unconditional ,
                                                ruleMatch = matchF ,
                                                ruleReplacement = constantReplacement
                                                     [ f, 
                                                       NodeBranch [[f]] , f ] } ) ,
                                                                  -- 33% chance F -> F[F]F
                                       (0.33, DeterministicRule {
                                                ruleContext = ignoreContext ,
                                                ruleCondition = unconditional ,
                                                ruleMatch = matchF ,
                                                ruleReplacement = constantReplacement
                                                     [ f,
                                                       NodeBranch [[p]], f ] } ),
                                                                  -- 33% chance F -> F[+]F
                                        (0.33, DeterministicRule {
                                                 ruleContext = ignoreContext ,
                                                 ruleCondition = unconditional ,
                                                 ruleMatch = matchF ,
                                                 ruleReplacement = constantReplacement
                                                      [ f,
                                                        NodeBranch [[f,f]],f] } ) ] ] ,
                                                                   -- 33% chance F -> F[FF]F
       systemSteps = 4 } where
               x = NodeDummy [] "X"
               f = NodeDraw [] 1
               p = NodeRotate [] (-12.5) 0 0
               m = NodeRotate [] 12.5 0 0
