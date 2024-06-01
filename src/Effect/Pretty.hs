module Effect.Pretty
  ( Pretty (..)
  , PrettyC (..)
  , runPretty
  , BlandC (..)
  , prettyStdErr
  , prettyStdOut
  , pretty
  )
where

import Control.Algebra (Algebra (..), Has, send, (:+:) (..))
import Control.Carrier.Reader (ReaderC (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Vector (Vector)
import System.IO (Handle, stderr, stdout)
import UnliftIO (MonadIO (..), MonadUnliftIO)

pretty :: forall spec sig m. Has (Pretty spec) sig m => Handle -> Vector (spec, Text) -> m ()
pretty hdl line = send $ PrettyLine hdl line

prettyStdErr :: Has (Pretty spec) sig m => Vector (spec, Text) -> m ()
prettyStdErr = pretty stderr

prettyStdOut :: Has (Pretty spec) sig m => Vector (spec, Text) -> m ()
prettyStdOut = pretty stdout

type Pretty :: Type -> (Type -> Type) -> Type -> Type
data Pretty spec m r where
  PrettyLine :: Handle -> Vector (spec, Text) -> Pretty spec m ()

newtype PrettyC spec m a = MkPrettyC {runPrettyC :: (spec -> Text -> Text) -> m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderC (spec -> Text -> Text) m

runPretty :: (spec -> Text -> Text) -> PrettyC spec m a -> m a
runPretty = flip runPrettyC

newtype BlandC spec m a = MkBlandC {runBland :: m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO) via IdentityT m

instance (Algebra sig m, MonadIO m) => Algebra (Pretty spec :+: sig) (PrettyC spec m) where
  alg hdl sig ctx =
    case sig of
      L (PrettyLine hdl' spec) ->
        ctx <$ MkPrettyC \colour ->
          liftIO $ T.hPutStrLn hdl' (uncurry colour `foldMap` spec)
      R other -> MkPrettyC \colour -> alg (runPretty colour . hdl) other ctx

instance (Algebra sig m, MonadIO m) => Algebra (Pretty spec :+: sig) (BlandC spec m) where
  alg hdl sig ctx = case sig of
    L (PrettyLine hdl' spec) ->
      ctx <$ MkBlandC do
        liftIO $ T.hPutStrLn hdl' $ foldMap snd spec
    R other -> MkBlandC $ alg (runBland . hdl) other ctx
