-- ======================================================================
-- Quantum Programming Series with Quipper by Oscar Riveros
-- Schrodinger's cat alive!
-- http://www.scribd.com/doc/174494333/Schrodinger%E2%80%99s-cat-alive
-- ======================================================================

import Quipper
import QuipperLib.Simulation
import QuipperLib.Arith
import Control.Monad (zipWithM)

eru :: Qubit -> Qubit -> Circ Qubit
eru qlive qdeath  = do
	live <- qinit True
	death <- qinit False
	label live "There was Eru, the One."
	label live "There was Eru, the Zero."
	qdeath <- qnot qdeath
	qlive <- qnot qlive
	label qdeath "God's dice"
	label qlive "God's dice"
	qnot_at live `controlled` [qlive, qdeath]
	qnot_at death `controlled` [qdeath, qlive]
	label live "Cat Live!"		
	live <- qnot live `controlled` death
	qdiscard [death,qlive,qdeath]
	return live

limbus :: QDInt -> QDInt -> Circ [Qubit]
limbus live death = do
	let qlive = qulist_of_qdint_lh live
	label qlive "Live"
 	let qdeath = qulist_of_qdint_lh death
 	label qlive "Death"
 	live <- zipWithM eru qlive qdeath 
 	return (map (\(nothing) -> nothing) live)

schrodingers_cat :: [Qubit] -> Circ [Qubit]
schrodingers_cat xs = do
	let cat = qdint_of_qulist_lh xs
	label cat "Cat IN Limbus"
	(live,death) <- q_negate cat
	qcat <- limbus live death
	label qcat "Cat OUT Limbus"	
	return qcat

black_box :: String -> Circ [Bit]
black_box scat  = do
	qcat <- qinit_of_string scat
	label qcat "Cat IN Box"
	cats <- schrodingers_cat qcat
	label cats "Cat OUT Box"
	cats' <- measure cats
	return cats'

main :: IO ()
main = do
	print_generic Preview black_box "1001"
	-- Cat State Nº 1
	out <- run_generic_io db black_box cat1
	putStrLn ("Cat In State Nº 1 Is Alive? = " ++ show out)
	-- Cat State Nº 2
	out <- run_generic_io db black_box cat2
	putStrLn ("Cat In State Nº 2 Is Alive? = " ++ show out)
	-- Cat State Nº 3
	out <- run_generic_io db black_box cat3
	putStrLn ("Cat In State Nº 3 Is Alive? = " ++ show out)
	-- Cat State Nº 4
	out <- run_generic_io db black_box cat4
	putStrLn ("Cat In State Nº 4 Is Alive? = " ++ show out)
	-- Cat State Nº 5
	out <- run_generic_io db black_box cat5
	putStrLn ("Cat In State Nº 5 Is Alive? = " ++ show out)
	-- Cat State Nº 6
	out <- run_generic_io db black_box cat6
	putStrLn ("Cat In State Nº 6 Is Alive? = " ++ show out)
	where
		db :: Double
		db = undefined
		cat1 :: String
		cat1 = "1100"
		cat2 :: String
		cat2 = "1010"
		cat3 :: String
		cat3 = "1001"
		cat4 :: String
		cat4 = "0110"
		cat5 :: String
		cat5 = "0101"
		cat6 :: String
		cat6 = "0011"