import Migration.Data (createTableSql)

main :: IO ()
main = do
  putStrLn "testing createTableSql..."
  let testSql = createTableSql "FilmRoll" ["Title text not null"
                                          , "DateCreated text not null"
                                          ]
  let actualSql = "create table if not exists FilmRoll (Title text not null, DateCreated text not null);"
  if testSql == actualSql then putStrLn "They are equal!" else putStrLn "Not equal!"
