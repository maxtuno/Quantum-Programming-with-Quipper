-- ======================================================================
-- Quantum Programming Series with Quipper by Oscar Riveros
-- Schrodinger's cat alive!
-- ======================================================================

import Quipper
import QuipperLib.Simulation
import QuipperLib.Arith
import Control.Monad (zipWithM)

eru :: Qubit -> Qubit -> Circ Qubit
eru qlive qdeath  = do
	live <- qinit True
	label live "There was Eru, the One."
	qdeath <- qnot qdeath
	qlive <- qnot qlive
	label qdeath "God's dice"
	label qlive "God's dice"
	qnot_at live `controlled` [qlive, qdeath]
	label live "Cat Live!"
	return live

limbus :: QDInt -> QDInt -> Circ [Qubit]
limbus live death = do
	let qlive = qulist_of_qdint_lh live
	label qlive "Live"
 	let qdeath = qulist_of_qdint_lh death
 	label qlive "Death"
 	w <- zipWithM eru qlive qdeath 
 	return (map (\(z) -> z) w)

schrodingers_cat :: [Qubit] -> Circ [Qubit]
schrodingers_cat xs = do
	let cat = qdint_of_qulist_lh xs
	label cat "Cat IN Limbus"
	(live,death) <- q_negate cat
	qcat <- limbus live death
	label qcat "Cat OUT Limbus"
	qdiscard [live,death]
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
	print_generic Preview black_box catsname  
	out <- run_generic_io db black_box catsname  
	putStrLn ("? = " ++ show out)
	where
		db :: Double
		db = undefined
		catsname :: String
		catsname = "1001"