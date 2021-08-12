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

normNF_OneStep (names, App left_expr right_expr) =
  let
    -- For Apps, first reduce the RHS, then LHS
    -- Reduce LHS only if RHS is not reduced (right_expr' == right_expr)
    -- because we only want to reduce one step
    default_f = \(names, expr) -> (names, expr)
    (right_names, right_expr') = maybe (names, right_expr) default_f (normNF_OneStep (names, right_expr))
    -- (left_names, left_expr') = maybe (right_names, left_expr) default_f (normNF_OneStep (right_names, left_expr))
    (left_names, left_expr') = if right_expr' == right_expr
      then maybe (right_names, left_expr) default_f (normNF_OneStep (right_names, left_expr))
      else (right_names, left_expr)
  in
    Just (left_names, App left_expr' right_expr')

normNF_OneStep (names, (Lambda name expr)) =
  maybe Nothing (\(names, expr) -> Just (names, Lambda name expr)) (normNF_OneStep (names, expr))

normNF_OneStep (names, expr) = Nothing


-- Part e
normNF_n :: Int -> ([Name], Expr) -> ([Name], Expr)
normNF_n 0 (names, expr) = (names, expr)
normNF_n n (names, expr)
  | n < 0 = (names, expr)
  | otherwise = maybe (names, expr) (\(names', expr') -> normNF_n (n - 1) (names', expr')) (normNF_OneStep (names, expr))

-- Part f
usedNames :: Expr -> [Name]
usedNames (Var name) = [name]
usedNames (App expr1 expr2) = (usedNames expr1) ++ (usedNames expr2)
usedNames (Lambda name expr) = usedNames expr

-- Part g
normNF :: Int -> Expr -> Expr
normNF n expr = snd (normNF_n n ([ show x | x <- [1..], not ((show x) `elem` (usedNames expr))], expr))

-- main driver function with non-exhaustive testcases
main = do
  -- a -> a
  putStrLn (show (normNF 10 (Var "a")))

  -- \a.a -> \a.a
  putStrLn (show (normNF 10 (Lambda "a" (Var "a"))))

  -- \a.a(b) -> b
  putStrLn (show (normNF 10 (App (Lambda "a" (Var "a")) (Var "b"))))

  -- \a.a(b) -> \a.a(b) .. 0 reductions
  putStrLn (show (normNF 0 (App (Lambda "a" (Var "a")) (Var "b"))))

  -- \a.a(b) -> \a.a(b) .. negative reductions same as 0 reductions
  putStrLn (show (normNF (-10) (App (Lambda "a" (Var "a")) (Var "b"))))

  -- \ab.a(c) -> \b.c
  putStrLn (show (normNF 10 (App (Lambda "a" (Lambda "b" (Var "a"))) (Var "c"))))

  -- \ab.ab(c) -> \b.cb
  putStrLn (show (normNF 10 (App (Lambda "a" (Lambda "b" (App (Var "a" ) (Var "b")))) (Var "c"))))

  -- \ab.ab(b) -> \1.b1
  putStrLn (show (normNF 10 (App (Lambda "a" (Lambda "b" (App (Var "a" ) (Var "b")))) (Var "b"))))

  -- \abc.abc(d)(f) -> \c.dfc
  putStrLn (show (normNF 10 (App (App (Lambda "a" (Lambda "b" (Lambda "c"
    (App (App (Var "a" ) (Var "b")) (Var "c"))))) (Var "d")) (Var "f"))))

  -- \abc.abc(b)(c) -> \2.bc2
  putStrLn (show (normNF 10 (App (App (Lambda "a" (Lambda "b" (Lambda "c"
    (App (App (Var "a" ) (Var "b")) (Var "c"))))) (Var "b")) (Var "c"))))

  -- \a.a(\b.b) -> \b.b
  putStrLn (show (normNF 10 (App (Lambda "a" (Var "a")) (Lambda "b" (Var "b")))))

  -- \a.a(\b.b(c)) -> c
  putStrLn (show (normNF 10 (App (Lambda "a" (Var "a")) (App (Lambda "b" (Var "b")) (Var "c")))))

  -- \a.a(\b.b(c)) -> \b.b(c) .. only one reduction
  putStrLn (show (normNF 1 (App (Lambda "a" (Var "a")) (App (Lambda "b" (Var "b")) (Var "c")))))

  -- \a.a(\b.b(c)) -> \a.a(\b.b(c)  .. no reductions
  putStrLn (show (normNF 0 (App (App (Lambda "a" (Var "a")) (Var "x")) (App (Lambda "b" (Var "b")) (Var "y")))))

  -- \a.a(\b.b(c)) -> \b.b(c) .. one reduction
  putStrLn (show (normNF 1 (App (App (Lambda "a" (Var "a")) (Var "x")) (App (Lambda "b" (Var "b")) (Var "y")))))

  -- \a.a(\b.b(c)) -> c .. two reduction
  putStrLn (show (normNF 2 (App (App (Lambda "a" (Var "a")) (Var "x")) (App (Lambda "b" (Var "b")) (Var "y")))))

  -- \a.aa (\a.aa) .. no normal form
  putStrLn (show (normNF 10 (App (Lambda "a" (App (Var "a") (Var "a")))
    (Lambda "a" (App (Var "a") (Var "a"))))))
