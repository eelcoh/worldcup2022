module Types.DataStatus exposing (..)

import Types exposing (DataStatus(..))


map : (a -> b) -> DataStatus a -> DataStatus b
map f status =
    case status of
        Fresh a ->
            f a
                |> Fresh

        Stale a ->
            f a
                |> Stale

        Filthy a ->
            f a
                |> Filthy


update : a -> DataStatus a -> DataStatus a
update data status =
    Fresh data
