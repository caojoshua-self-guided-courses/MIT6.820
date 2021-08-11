data Expr =
    Var Name -- a variable
    | App Expr Expr -- application
    | Lambda Name Expr -- lambda abstraction
  deriving
    (Eq, Show) -- use default compiler generated Eq and Show instances

type Name = String

-- Part c
-- replaces free vars
replaceVar :: (Name, Expr) -> Expr -> Expr
replaceVar (n, e) (Var name)
  | n == name = e
  | otherwise = Var name
replaceVar (n, e) (Lambda name expr)
  | n == name = Lambda name expr
  | otherwise = Lambda name (replaceVar (n, e) expr)
replaceVar (n, e) (App expr1 expr2) = App (replaceVar (n, e) expr1) (replaceVar (n, e) expr2)


-- Part d

-- helper function to replace bound variables that are used in the application
replaceBoundVars :: ([Name], [Name]) -> Expr -> ([Name], Expr)
replaceBoundVars (app_names, unused_names) (Lambda name expr)
  | name `elem` app_names = (tail unused_names,
    Lambda (head unused_names) (snd (tail unused_names, replaceVar (name, Var (head unused_names)) expr)))
  | otherwise = (unused_names, Lambda name expr)
replaceBoundVars (_, unused_names) expr = (unused_names, expr)

normNF_OneStep :: ([Name], Expr) -> Maybe ([Name], Expr)
normNF_OneStep (names, (App (Lambda name lambda_expr) app_expr)) =
  let
    (unused_names, new_lambda_expr) = replaceBoundVars (usedNames app_expr, names) lambda_expr
  in
    Just (unused_names, replaceVar (name, app_expr) new_lambda_expr)
normNF_OneStep (names, (Lambda name expr)) =
  maybe Nothing (\(names, expr) -> Just (names, Lambda name expr)) (normNF_OneStep (names, expr))
normNF_OneStep (names, expr) = Nothing

-- Part e
-- assumes non-negative as input in n
normNF_n :: Int -> ([Name], Expr) -> ([Name], Expr)
normNF_n 0 (names, expr) = (names, expr)
normNF_n n (names, expr) =
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
  putStrLn (show (normNF 10 (App (Lambda "a" (Lambda "b" (Var "a"))) (Var "c"))))
  putStrLn (show (normNF 10 (App (Lambda "a" (Lambda "b" (App (Var "a" ) (Var "b")))) (Var "c"))))
  putStrLn (show (normNF 10 (App (Lambda "a" (Lambda "b" (App (Var "a" ) (Var "b")))) (Var "b"))))
