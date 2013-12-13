import Validation
import Control.Applicative

validatePassword pw = if length pw > 6 
                        then valid pw 
                        else invalid pw ["password too short"]

validateUsername un = if length un > 6 
                        then valid un 
                        else invalid un ["username too short"]

-- validate using Applicative
validateAccount user pass = (,) <$> validateUsername user<*> validatePassword pass 

-- validate using Monad
validateMonadic user pass = do
  validateUsername user
  validatePassword pass
  return (user, pass)
