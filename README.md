data-serialization-postgresql
=============================

Simply derive your type from <code>Generic</code>, <code>Decoding FromFields</code> and <code>Encoding ToFields</code>.
If you want some field not to present in query, use <code>OptField</code> wrapper.

<pre>
data Test = Test {
    testInt :: Int, -- ^ This field must be in query
    testOptional :: Maybe Double, -- ^ This field must be in query, but it's nullable
    testString :: OptField String } -- ^ This may not be in query
        deriving (Generic, Show)

instance Serializable (Decoding FromFields) Test
instance Serializable (Encoding ToFields) Test
</pre>

You can collect field names with <code>gfields</code> function:

<pre>
testFields :: [String]
testFields = fields (gfields :: Fields Test)
-- ["testInt","testOptional","testString"]
</pre>

Example of usage:

<pre>
create :: IO ()
create = do
    con &lt;- connect testcon
    execute_ con "create table test (id integer, value double precision, name text)"
    return ()

runInsert :: IO ()
runInsert = do
    con &lt;- connect testcon
    let
        acts = either undefined id $ encodeRow (Test 123 Nothing (Has "Hello, world!"))
    print acts
    -- [Plain "123",Plain "null",Escape "Hello, world!"]
    execute con "insert into test values (?, ?, ?)" acts
    return ()

runSelect :: IO ()
runSelect = do
    con &lt;- connect testcon
    [ff] &lt;- query_ con "select * from test limit 1"
    print $ (decodeRow ff :: Either String Test)
    -- Right (Test {testInt = 123, testOptional = Nothing, testString = Has "Hello, world!"})
</pre>
