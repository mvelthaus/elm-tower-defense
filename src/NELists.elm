-- Author: Mirko Velthaus


module NELists exposing (..)

import Lists


type NEList a
    = Head a (List a)



-- create a non-empty list with n identical elements


repeat : Int -> a -> NEList a
repeat n x =
    Head x (Lists.repeat n x)



-- add an element at the end


snoc : NEList a -> a -> NEList a
snoc (Head x xs) e =
    Head x (Lists.snoc xs e)



-- convert NEList to List


toList : NEList a -> List a
toList (Head x xs) =
    x :: xs



-- remove the last element


removeLast : NEList a -> List a
removeLast l =
    foldr (\x r -> x :: r) (\_ -> []) l



-- get the last element


last : NEList a -> a
last l =
    foldr (\_ r -> r) (\y -> y) l


foldr : (a -> b -> b) -> (a -> b) -> NEList a -> b
foldr f g (Head x xs) =
    case xs of
        [] ->
            g x

        y :: ys ->
            f x (foldr f g (Head y ys))
