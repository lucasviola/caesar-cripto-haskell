import Data.List
import Data.Char
import Data.Function

criptografaMensagem :: Int -> String -> String  
criptografaMensagem shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted 


-- Usando Composição de Tipos
criptografaMensagem' :: Int -> String -> String
criptografaMensagem' shift msg = map (chr . (+ shift) . ord) msg

-- Descriptografa criptografaMensagem
descriptografaMensagem :: Int -> String -> String
descriptografaMensagem shift msg = criptografaMensagem (negate shift) msg
