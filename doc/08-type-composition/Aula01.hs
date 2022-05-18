module Aula01 where

import Control.Monad (join)

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)



-- Functors --

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap.fmap) f fga


-- Applicatives --

instance Applicative Identity where
  -- pure :: a -> Identity a
  pure = Identity

  -- (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  -- (Identity f) <*> (Identity a) = Identity (f a)
  (Identity f) <*> ia = fmap f ia


instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  -- pure :: a -> Compose f g a
  pure a = Compose $ (pure.pure) a

  -- (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fg_ab) <*> (Compose fga) = Compose $
    fmap (<*>) fg_ab <*> fga


-- Transformers --

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor f) => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative f) => Applicative (IdentityT f) where
  pure x = IdentityT $ pure x
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa
  
instance (Monad m) => Monad (IdentityT m) where
  -- (>>=) :: IdentityT m a ->
  --       -> (a -> IdentityT m b)
  --       -> IdentityT m b
 (IdentityT ma) >>= f =
   IdentityT $ ma >>= runIdentityT . f


-- MaybeT monad transformer --

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap.fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ (pure.pure) a
  
  (MaybeT mab) <*> (MaybeT mma) =
    MaybeT $ fmap (<*>) mab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  (MaybeT ma) >>= f =
    MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)


-- EitherT/ErrorT monad transormer --

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

instance (Functor m) => Functor (ErrorT e m) where
  fmap f (ErrorT m_eea) = ErrorT $ (fmap.fmap) f m_eea

instance (Applicative m) => Applicative (ErrorT e m) where
  pure x = ErrorT $ (pure.pure) x

  (ErrorT f) <*> (ErrorT e) =
    ErrorT $ fmap (<*>) f <*> e

instance (Monad m) => Monad (ErrorT e m) where
  (ErrorT m_eea) >>= f =
    ErrorT $ do
    v <- m_eea
    case v of
      (Left e) -> return (Left e)
      (Right a) -> runErrorT (f a)


-- ReaderT monad transformer --

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap.fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ (pure.pure) a

  (ReaderT fmab) <*> (ReaderT rma) =
    ReaderT $ fmap (<*>) fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r


-- StateT monad transformer --

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT st) = StateT $ \s ->
    fmap (\(a,s') -> (f a, s')) $ st s

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a,s)

  (StateT g) <*> (StateT x) =
    StateT $ \s -> do
    (f, s') <- g s
    (a, s'') <- x s'
    return (f a, s'')
    



