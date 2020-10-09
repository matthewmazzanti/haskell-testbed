{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.Freer
import Control.Monad.Freer.Coroutine

test :: Member (Yield Integer Integer) effs => Eff effs Integer
test = yield 10 (+1)

testRun = run $ runC test
