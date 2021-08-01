-- A more functional version for practice
-- diff f dx x = (/) ((-) (f ((+) x dx)) (f x)) dx

diff f dx x = (f (x + dx) - f x) / dx

-- idk why they want us to have f' as an argument if we're just going to use
-- newton approximation but hey just completing the assignment
newton_iter f f' x 0 = x - 1 
newton_iter f f' x k = 
  let
    xi = x - 1
  in
    xi - f xi / diff f 0.01 xi

problem f f' x = show(newton_iter f f' x (f x))

main =
  let f2 x = x ^ 3 - 328 * x ^ 2 - 1999 * x - 1670
      f2' x = 3 * x ^ 2 - 656 * x - 1999
  in
    putStrLn(
    (problem sin cos 0.5) ++ "\n" ++
    (problem f2 f2' 100)
    )
