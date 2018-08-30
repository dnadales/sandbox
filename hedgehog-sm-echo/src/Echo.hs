-- | Echo API.

module Echo (Echo, input, output, reset) where

class Echo e where
    -- | Input a string. Returns 'True' iff the buffer was empty and the given
    -- string was added to it.
    input :: e -> String -> IO Bool

    -- | Output the buffer contents (if any).
    output :: e -> IO (Maybe String)

    -- | Reset the echo state.
    reset :: e -> IO ()
