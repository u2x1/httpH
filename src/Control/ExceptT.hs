module Control.ExceptT where

class MonadTrans t where
  lift :: Monad m => m a -> t m a

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance MonadTrans (ExceptT e) where
  lift = ExceptT . (fmap Right)

instance Monad m => Monad (ExceptT e m) where
  return = ExceptT . return . Right
  x >>= f = ExceptT $ do
    either_val <- runExceptT x
    case either_val of
      Right val -> runExceptT (f val)
      Left err  -> return $ Left err

instance Monad m => Applicative (ExceptT e m) where
  pure = return
  f <*> x = do
    f' <- f
    x' <- x
    return (f' x')

instance Monad m => Functor (ExceptT e m) where
  fmap f x = x >>= (return . f)

except :: Monad m => e -> ExceptT e m a
except = ExceptT . pure . Left