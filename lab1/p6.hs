data Expr =
    Var Name -- a variable
    | App Expr Expr -- application
    | Lambda Name Expr -- lambda abstraction
  deriving
    (Eq, Show) -- use default compiler generated Eq and Show instances

type Name = String

-- Part c
replaceVar :: (Name, Expr) -> Expr -> Expr
replaceVar (n, e) (Var name)
  | n == name = e
  | otherwise = Var name
replaceVar (n, e) (Lambda name expr)
  | n == name = Lambda name expr
  | otherwise = Lambda name (replaceVar (n, e) expr)
replaceVar (n, e) (App expr1 expr2) = App (replaceVar (n, e) expr1) (replaceVar (n, e) expr2)

-- Part d

-- helper function to replace all free variables in a Lambda Expr
-- returns a tuple of the remaining unused names and the resulting Expr
replaceLambdaVars :: ([Name], [Name]) -> Expr -> ([Name], Expr)
replaceLambdaVars (app_names, unused_names) (Lambda name expr)
  | length app_names == 0 = (unused_names, Lambda name expr)
  | length unused_names == 0 = (unused_names, Lambda name expr) 
  | otherwise = replaceLambdaVars (tail app_names, tail unused_names)
      (replaceVar (head app_names, Var (head unused_names)) (Lambda name expr))
replaceLambdaVars (names, _) expr = (names, expr)

-- TODO: replaceLambdaVars for non-free variables in lambda expr
normNF_OneStep :: ([Name], Expr) -> Maybe ([Name], Expr)
normNF_OneStep (names, (App (Lambda name lambda_expr) app_expr)) =
  -- Just (replaceLambdaVars (usedNames app_expr, names) lambda_expr)
  Just (names, replaceVar (name, app_expr) lambda_expr)
normNF_OneStep (names, (Lambda name expr)) =
  maybe Nothing (\(names, expr) -> Just (names, Lambda name expr)) (normNF_OneStep (names, expr))
normNF_OneStep (names, expr) = Nothing

-- Part e
-- assumes non-negative as input in n
normNF_n :: Int -> ([Name], Expr) -> ([Name], Expr)
normNF_n 0 (names, expr) = (names, expr)
normNF_n n (names, expr) =
  -- maybe (names, expr) (\(names', expr') -> (names', expr')) (normNF_OneStep (names, expr))
  maybe (names, expr) (\(names', expr') -> normNF_n (n - 1) (names', expr')) (normNF_OneStep (names, expr))

-- Part f
usedNames :: Expr -> [Name]
usedNames (Var name) = [name]
usedNames (App expr1 expr2) = (usedNames expr1) ++ (usedNames expr2)
usedNames (Lambda name expr) = usedNames expr

-- Part g
normNF :: Int -> Expr -> Expr
normNF n expr = snd (normNF_n n ([ show x | x <- [1..], not ((show x) `elem` (usedNames expr))], expr))

main = do
  putStrLn (show (normNF 10 (Var "a")))
  putStrLn (show (normNF 10 (Lambda "a" (Var "a"))))
  putStrLn (show (normNF 10 (App (Lambda "a" (Var "a")) (Var "b"))))
