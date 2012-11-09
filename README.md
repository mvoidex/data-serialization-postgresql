data-serialization-postgresql
=============================

Simply derive your type from @Generic@, @Decoding FromFields@ and @Encoding ToFields@.
If you want some field not to present in query, use @OptField@ wrapper.

<pre>
data Test = Test {
    testInt :: Int, -- ^ This field must be in query
    testOptional :: Maybe Double, -- ^ This field must be in query, but it's nullable
    testString :: OptField String } -- ^ This may not be in query
        deriving (Generic, Show)

instance Serializable (Decoding FromFields) Test
instance Serializable (Encoding ToFields) Test
instance Serializable Fields Test
instance InTable Test where
    table _ = "test"
</pre>

You can specify names for columns:

<pre>
instance Serializable Pgser Test where
    ser =
        dat_ (ctor_ (
            stor "id" ser .*.
            stor "opti" ser .*.
            stor "teststr" ser))
        .:.
        giso
</pre>

Example:

<pre>
runCreate :: IO ()
runCreate = do
    con &lt;- connect testcon
    execute_ con "drop table test"
    create con (Table :: Table Test)
    return ()

runInsert :: IO ()
runInsert = do
    con &lt;- connect testcon
    insert con (Test 1 Nothing (Has "Hello, world!"))
    insert con (Test 2 (Just 10.0) (Has "Some string"))
    insert con (Test 3 Nothing HasNo)
    -- Test {testInt = 1, testOptional = Nothing, testString = Has "Hello, world!"}
    -- Test {testInt = 2, testOptional = Just 10.0, testString = Has "Some string"}
    -- Test {testInt = 3, testOptional = Nothing, testString = HasNo}
    return ()

runUpdate :: IO ()
runUpdate = do
    con &lt;- connect testcon
    -- @Nothing@ is for null, @HasNo@ is for no update
    update_ con (Test 1 (Just 20.0) HasNo) " where testint = 1"
    update_ con (Test 2 Nothing (Has "New string")) " where testint = 2"
    update_ con (Test 3 (Just 30.0) (HasNo)) " where testint = 3"
    -- Test {testInt = 1, testOptional = Just 20.0, testString = Has "Hello, world!"}
    -- Test {testInt = 2, testOptional = Nothing, testString = Has "New string"}
    -- Test {testInt = 3, testOptional = Just 30.0, testString = HasNo}
    return ()

runSelect :: IO ()
runSelect = do
    con &lt;- connect testcon
    vs &lt;- select_ con "" :: IO [Test]
    mapM_ print vs
</pre>
