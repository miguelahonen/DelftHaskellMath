{-# START_FILE main.hs #-}
import Language.C

main = do
    result <- parseCFilePre "test.c"
    case result of
        Left error -> print error
        Right ast -> do
            print ast
            print $ pretty ast
{-# START_FILE test.c #-}
int main() {
    printf("Hello, world!\n");
    return 0;
}
