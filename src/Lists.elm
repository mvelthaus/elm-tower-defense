-- Author: Mirko Velthaus


module Lists exposing (..)

import Tuple



-- create a list with n identical elements


repeat : Int -> a -> List a
repeat c x =
    if c > 0 then
        x :: repeat (c - 1) x

    else
        []



-- get the last element


last : List a -> Maybe a
last l =
    case l of
        [] ->
            Nothing

        s :: [] ->
            Just s

        _ :: xs ->
            last xs



-- remove the last element


removeLast : List a -> List a
removeLast l =
    case l of
        [] ->
            []

        _ :: [] ->
            []

        x :: xs ->
            x :: removeLast xs



-- uses a given function on every element


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap f l =
    let
        countedMapLocal c =
            foldl (\x ( r, s ) -> ( f s x :: r, s + 1 )) ( [], c ) l
    in
    Tuple.first (countedMapLocal 0)



-- add an element at the end


snoc : List a -> a -> List a
snoc l x =
    foldr (\y r -> y :: r) [ x ] l



-- uses a function returning a bool on every element


any : (a -> Bool) -> List a -> Bool
any p l =
    foldl (\x r -> p x || r) False l


length : List a -> Int
length l =
    case l of
        [] ->
            0

        _ :: xs ->
            1 + length xs


foldr : (a -> b -> b) -> b -> List a -> b
foldr f e l =
    case l of
        [] ->
            e

        x :: xs ->
            f x (foldr f e xs)


foldl : (a -> b -> b) -> b -> List a -> b
foldl f e l =
    case l of
        [] ->
            e

        x :: xs ->
            foldl f (f x e) xs
