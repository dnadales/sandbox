

module ComputeTheSize where


import Data.Data

data Foo = Foo deriving (Data, Typeable)

data Bar = Bar deriving (Data, Typeable)

data Baz = Baz [Foo] (Bar, Bar) deriving (Data, Typeable)

newtype Size a = Size { unSize :: Int }

plus :: Size a -> Size b -> Size c
plus (Size s0) (Size s1) = Size (s0 + s1)

sumFields :: Data a => a -> Size a
sumFields baz = gfoldl step fcstr baz
  where
    step :: forall d b. Data d =>  Size (d -> b) -> d -> Size b
    step tot d = tot `plus` sumFields d

    fcstr :: forall g. g -> Size g
    fcstr _  = Size 0
