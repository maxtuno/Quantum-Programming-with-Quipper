-- ======================================================================
-- Quantum Programming Series with Quipper by Oscar Riveros
-- ======================================================================

import Quipper
import QuipperLib.Simulation
import QuipperLib.Arith
import Algorithms.USV.Definitions

circuit :: [Qubit] -> [Qubit] -> [Qubit] -> [Qubit] -> Circ [QDInt]
circuit xs ys zs ws = do
	let ix = qdint_of_qulist_lh xs
	let iy = qdint_of_qulist_lh ys
	let iz = qdint_of_qulist_lh zs
	let iw = qdint_of_qulist_lh ws
	ixyzws <- q_vector_add [ix,iy] [iz,iw]
	return ixyzws

main_circuit :: String -> String -> String -> String -> Circ [QDInt]
main_circuit sx sy sz sw = do
	xs <- qinit_of_string sx
	ys <- qinit_of_string sy
	zs <- qinit_of_string sz
	ws <- qinit_of_string sw
	is <- circuit xs ys zs ws
	return is

main :: IO ()
main = do
	print_generic Preview main_circuit aux0 aux1 aux2 aux3
	out <- run_generic_io db main_circuit aux0 aux1 aux2 aux3
	putStrLn ("main_circuit = " ++ show out)
	where
		db :: Double
		db = undefined
		aux0 :: String
		aux0 = "0"
		aux1 :: String
		aux1 = "1"
		aux2 :: String
		aux2 = "+"
		aux3 :: String
		aux3 = "-"