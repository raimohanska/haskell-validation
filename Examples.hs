import Validation
import Control.Applicative

validatePassword :: String -> Validation [String] String
validatePassword pw = if length pw > 6 
                        then valid pw 
                        else invalid pw ["password too short"]

validateUsername :: String -> Validation [String] String
validateUsername un = if length un > 6 
                        then valid un 
                        else invalid un ["username too short"]

-- validate using Applicative
validateAccount user pass = (,) <$> validateUsername user<*> validatePassword pass 

-- validate using Monad
validateMonadic :: String -> String -> Validation [String] (String, String)
validateMonadic user pass = do
  validateUsername user
  validatePassword pass
  return (user, pass)
