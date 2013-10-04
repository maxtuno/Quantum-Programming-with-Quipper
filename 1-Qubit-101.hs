-- ======================================================================
-- Quantum Programming Series with Quipper by Oscar Riveros
-- 1-Qubit-101
-- ======================================================================

import Quipper
import QuipperLib.Simulation

circuit :: Qubit -> Circ Qubit
circuit x = do
	label x "x"

	y <- qinit False
	label y "y"

	x <- hadamard x
	x <- qnot x `controlled` y

	return x

main :: IO ()
main = do
	print_simple Preview circuit

	out <- run_generic_io db circuit False
	putStrLn ("circuit False = " ++ show out)

	out <- run_generic_io db circuit True
	putStrLn ("circuit True = " ++ show out)

	where
		db :: Double
		db = undefined