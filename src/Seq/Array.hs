{-# LANGUAGE MagicHash, UnboxedTuples #-}

import Prelude hiding
    ( head
    , tail
    , last
    , init
    , truncate
    , take
    , drop
    )

import GHC.Base
import GHC.Prim
import GHC.ST

data Array e = Array (Array# e)

instance Semigroup (Array e) where
  (<>) (Array arr1#) (Array arr2#) = runST (ST $ \s1# ->
      case newArray# (len1# +# len2#) arrBottom s1# of {(# s2#, marr# #) ->
      case copyArray# arr1# 0# marr# 0# len1# s2# of {s3# ->
      case copyArray# arr2# 0# marr# len1# len2# s3# of {s4# ->
      case unsafeFreezeArray# marr# s4# of {(# s5#, arr# #) ->
      (# s5#, Array arr# #)
      }}}})
    where len1# = sizeofArray# arr1#
          len2# = sizeofArray# arr2#

instance Monoid (Array e) where
  mempty = runST (ST $ \s1# ->
      case newArray# 0# arrBottom s1# of {(# s2#, marr# #) ->
      case unsafeFreezeArray# marr# s2# of {(# s3#, arr# #) ->
      (# s3#, Array arr# #)
      }})

instance Functor Array where
  fmap fn (Array arr#) = runST (ST $ \s1# ->
      case newArray# len# arrBottom s1# of {(# s2#, marr# #) ->
      case setValues 0# marr# s2# of {s3# ->
      case unsafeFreezeArray# marr# s3# of {(# s4#, arr# #) ->
      (# s4#, Array arr# #)
      }}})
    where
      len# = sizeofArray# arr#

      setValues i# marr# = \s1# ->
          case i# >=# len# of
              1# -> s1#
              0# ->
                  case indexArray# arr# i# of {(# elem #) ->
                  case writeArray# marr# i# (fn elem) s1# of {s2# ->
                  setValues (i# +# 1#) marr# s2#
                  }}

instance Applicative Array where
  pure elem = runST (ST $ \s1# ->
      case newArray# 1# elem s1# of {(# s2#, marr# #) ->
      case unsafeFreezeArray# marr# s2# of {(# s3#, arr# #) ->
      (# s3#, Array arr# #)
      }})

  (<*>) (Array arr1#) (Array arr2#) = runST (ST $ \s1# ->
      case newArray# (len1# *# len2#) arrBottom s1# of {(# s2#, marr# #) ->
      case setValues 0# marr# s2# of {s3# ->
      case unsafeFreezeArray# marr# s3# of {(# s4#, arr# #) ->
      (# s4#, Array arr# #)
      }}})
    where
      len1# = sizeofArray# arr1#
      len2# = sizeofArray# arr2#

      setValues i# marr# = \s1# ->
          case i# >=# len1# of
              1# -> s1#
              0# ->
                  case indexArray# arr1# i# of {(# fn #) ->
                  case setValues' fn i# 0# marr# s1# of {s2# ->
                  setValues (i# +# 1#) marr# s2#
                  }}

      setValues' fn i# j# marr# = \s1# ->
          case j# >=# len2# of
              1# -> s1#
              0# ->
                  case indexArray# arr2# j# of {(# elem #) ->
                  let offset# = (len1# *# i#) +# j# in
                  case writeArray# marr# offset# (fn elem) s1# of {s2# ->
                  setValues' fn i# (j# +# 1#) marr# s2#
                  }}


arrBottom :: e
arrBottom = errorWithoutStackTrace "Bottom value encounted in array"


fromList :: [e] -> Array e
fromList l = runST (ST $ \s1# ->
    case newArray# len# arrBottom s1# of {(# s2#, marr# #) ->
    case setValues marr# 0# l s2# of {s3# ->
    case unsafeFreezeArray# marr# s3# of {(# s4#, arr# #) ->
    (# s4#, Array arr# #)
    }}})
  where
    (I# len#) = Prelude.length l

    setValues :: MutableArray# s e -> Int# -> [e] -> State# s -> State# s
    setValues _ _ [] = \s# -> s#
    setValues marr# i# (e:es) = \s1# ->
        case writeArray# marr# i# e s1# of {s2# ->
        setValues marr# (i# +# 1#) es s2#
        }

toList :: Array e -> [e]
toList arr = toList' arr 0 (arrLength arr)
  where
    toList' :: Array e -> Int -> Int -> [e]
    toList' arr i len
      | i < len = unsafeIndex arr i:toList' arr (i+1) len
      | otherwise = []


(!) :: Array e -> Int -> Maybe e
(!) arr@(Array arr#) i
  | len == 0  = Nothing
  | i >= len  = Nothing
  | i < 0     = Nothing
  | otherwise = Just (unsafeIndex arr i)
  where len = arrLength arr

unsafeIndex :: Array e -> Int -> e
unsafeIndex (Array arr#) (I# i#) = case indexArray# arr# i# of
    (# elem #) -> elem

arrLength :: Array e -> Int
arrLength (Array arr#) = I# (sizeofArray# arr#)
