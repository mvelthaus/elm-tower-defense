-- Author: Mirko Velthaus


module Lists exposing (any)

-- uses a function returning a bool on every element


any : (a -> Bool) -> List a -> Bool
any p l =
    List.foldl (\x r -> p x || r) False l
