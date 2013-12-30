-- | A monad-based version of binary_search,
--   because we need to run the build each time we want to test a version.
module Search where


-- | The first action which succeeds, assuming all later action also do.
-- >>> binary_search 4 [[Nothing], [Nothing], [Just 2], [Just 3]]
-- [2]
-- 
-- >>> binary_search 4 [[Nothing], [Nothing], [Nothing], [Nothing]]
-- [4]
-- 
-- prop> let n = x1 `mod` 10; k = if n > 0 then x2 `mod` n else 0; xs = [[Nothing] | x <- [0..k-1]] ++ [[Just x] | x <- [k..n-1]] in binary_search n xs == [k]
binary_search :: Monad m => a -> [m (Maybe a)] -> m a
binary_search d [] = return d
binary_search d mxs = do
    r <- mx
    case r of
      Nothing -> binary_search d mxs2
      Just x  -> binary_search x mxs1
  where
    i = length mxs `div` 2
    (mxs1, mx:mxs2) = splitAt i mxs
